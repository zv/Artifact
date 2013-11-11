defmodule Artifact.Hash do
  @compile {:nowarn_unused_function, [bucket_members: 4]}
  @docmodule """
  This module provides functions for creating new nodes and storing the
  keyspace assigned to those nodes into the underlying storage backend.

  ## Hashing Scheme
  Artifact's hashing scheme is accomplished through the use of a cheap hash
  function such as Murmur (Murmur is general purpose and very quick quicker,
  however some distribution scenarios can be impacted by a cryptographically
  insecure key distribution function, so Artifact provides it) to form a
  'ring'. The concept of a hashing ring is simply a mapping of the output space
  of a hash function onto a circular structure where nodes are assigned in an
  even fashion within the space. Each data item identified by a key is assigned
  to a node by hashing the data item’s key to yield its position on the ring,
  and then walking the ring to find the first node with a hash larger than the
  item’s. Each node therefore need only be concerned about the items which fall
  between it's hash boundaries of it's position on the ring and it's
  predecessor's. This is a significant advantage over other approaches like
  taking the modulo of the key because the arrival of a new node will only
  cause at most two nodes to begin reconciliatoin while other nodes may
  continue processing data normally.

  * http://dl.acm.org/citation.cfm?id=258660
  """
  use GenServer.Behaviour
  @doc """
    Load in our crypto module at compile time, we expect it to be wrap it's
    "hashing" functionality in a method called `hash`. Any hash function
    returning a binary of length 32 or greater can be used here, although
    cryptographically secure hash functions are more readily suited for for
    distributed hash storage in an insecure environment.
  """
  @hash_function :application.get_env(:artifact, :hash_function)
  @hash_length 32
  @doc
  def crypto(hash) do
    apply(Keyword.get(@hash_function, :module), Keyword.get(@hash_function, :fun), [hash])
  end
  def hash(key) do
    <<output :: @hash_length, _ :: binary>> = crypto(key)
    output
  end
  def hash({{n1,n2,n3,n4}, port}, vnode) do
    <<hashed_key :: @hash_length, _rest :: binary>> =
        crypto(<< n1,n2,n3,n4, port :: 16, vnode:: 16 >>)
      hashed_key
  end

  @doc false
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], _options = [])
  end

  @doc false
  def init(_args) do

    :ets.new :buckets        , [:set         , :private , :named_table]
    :ets.new :node_manifest  , [:set         , :private , :named_table]
    :ets.new :vnode_manifest , [:ordered_set , :private , :named_table]

    { :ok, [] }
  end

  @doc false
  def terminate(_reason, _state) do
    :ets.delete(:node_manifest)
    :ets.delete(:vnode_manifest)
    :ets.delete(:buckets)

    :ok
  end

  """
  List nodes whose mapped keyspace falls on this key_hash
  """
  def bucket_members(_key_hash, _n, i, nodes) when i == 0 do
    {:nodes, Enum.reverse(nodes)}
  end

  def bucket_members(key_hash, n, i, nodes) do
    node_hash = case :ets.next(:vnode_manifest, key_hash) do
      "$end_of_table" -> :ets.first(:vnode_manifest)
      other           -> other
    end
    [{_k,node} | _] = :ets.lookup(:vnode_manifest, node_hash)
    new_nodes = case Enum.member?(nodes, node) do
      true  -> nodes
      false -> [ node | nodes ]
    end

    case length(new_nodes) do
      ^n  -> {:nodes, Enum.reverse(new_nodes)}
      _   -> bucket_members(key_hash, n, i-1, new_nodes)
    end
  end

  @doc """
    Calculates the hash ring output space
  """
  def ring_circumference(bucket_count) do
    trunc(:math.pow(2, @hash_length) / bucket_count)
  end

  @doc """
    Fetches & writes another virtual node entry into the global vnode manifest.
    No locks are acquired so each manifest is node distinct .
  """
  def add_nodes(nodes) when nodes == [] do
    :ok
  end
  def add_nodes([{node, info}|tail]) do
    case :ets.lookup(:node_list, node) do
      [ {^node, _info} | _ ] -> :ok
      [] ->
        :ets.insert(:node_list, {node, info})
        vnode_count = Keyword.get(:vnodes, info)
        Enum.each(1..vnode_count,
          &:ets.insert(:vnode_manifest, {hash(node, &1), node})
        )
    end
    add_nodes(tail)
  end

  """
  update_buckets is a helper function to rearrange the buckets when a new node
  is added.
  """
  defp update_buckets(-1, _node, _range, _n, _max_search, reorganized_buckets) do
    {:reorganized_buckets, reorganized_buckets}
  end

  defp update_buckets(bucket, node, circumference, n, max_search, reorganized_buckets) do
    {:nodes, new_nodes} =
      bucket_members(bucket * circumference, n, max_search, [])

    case :ets.lookup(:buckets, bucket) do
      # If we found a preexising bucket, we can safely skip this.
      [ { ^bucket, ^new_nodes } ] -> update_buckets(bucket - 1, node, circumference,
                                                   n, max_search, reorganized_buckets )
      # Otherwise we have to insert a new bucket
      old_bucket ->
        :ets.insert(:buckets, {bucket, new_nodes})
        new_replica = Enum.find_index(node, new_nodes)
        old_replica = case old_bucket do
          [{bucket, old_nodes}] -> Enum.find_index(node, old_nodes)
          []                    -> :undefined
        end

        reorganized_buckets_n = if new_replica == old_replica do
          reorganized_buckets
        else
          [ { bucket, new_replica, old_replica } | reorganized_buckets ]
        end

        update_buckets(bucket - 1, node, circumference,
                       n, max_search, reorganized_buckets_n)
    end
  end

  defp update_buckets do
    [node, n, number_of_buckets] =
      :artifact_config.get [:node, :n, :number_of_buckets]

    circumference    = ring_circumference(number_of_buckets)
    number_of_nodes  = Keyword.get(:size, :ets.info :node_list )

    max_search      = case number_of_nodes do
      # Don't search other nodes to fill a bucket when NumberOfNodes is
      # 1, since they are never found.
      1    -> 1
      _    -> Keyword.get(:size, :ets.info :virtual_node_list)
    end
    update_buckets(number_of_buckets - 1, node, circumference, n, max_search, [])
  end

  def remove_nodes([]), do: :ok

  def remove_nodes([node | rest]) do
    case :ets.lookup(:node_manifest, node) do
      [ { node, info } | _ ] ->
          :ets.delete(:node_manifest, node)
          vnode_count = Keyword.get(:vnodes, info)
          Enum.each 1..vnode_count, fn(vnode) ->
            :ets.delete(:vnodes, hash(node, vnode))
          end
      [] -> :ok
    end
    remove_nodes(rest)
  end

  defp bucket_index(bucket, count) when is_integer(bucket) do
    rem(bucket, count)
  end

  defp bucket_index(key, count) do
    hash(key) / ring_circumference(count)
  end

  @doc """
  Find a bucket by either a constituent key or the bucket id
  """
  @spec locate_bucket(integer(), atom()) :: tuple()
  @spec locate_bucket(binary(), atom())  :: tuple()
  def locate_bucket(query, state) do
    bcount = Config.get(:buckets)

    {:reply, {:bucket, bucket_index(query, bcount)}, state}
  end

  @doc """
  Find a replica by a constituent key or bucket
  """
  @spec locate_replica(integer(), atom()) :: tuple()
  @spec locate_replica(binary(), atom())  :: tuple()
  def locate_replica(query, state) do
    node = Config.get(node)
    {:reply, {:nodes, nodes}, state2} = locate_nodes(query, state)

    {:reply, {:replica, Enum.find_index(node, nodes)}, state2}
  end

  @doc """
  Find a node either by a constituent bucket or key
  """
  @spec locate_nodes(integer(), atom()) :: tuple()
  @spec locate_nodes(binary(), atom())  :: tuple()
  def locate_nodes(query, state) do
    bcount  = Config.get(:buckets)
    bucket = bucket_index(query, bcount)
    [ { bucket, nodes } | _ ] = :ets.lookup(:buckets, bucket)

    {:reply, {:nodes, nodes}, state}
  end

  defp bucket_index(bucket, count) when is_integer(bucket) do
    rem(bucket, count)
  end
  defp bucket_index(key, count) do
    hash(key) / ring_circumference(count)
  end
end
