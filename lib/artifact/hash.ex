defmodule Artifact.Hash do
  @moduledoc """
  A set of functions for creating new nodes, storing and searching the keyspace
  assigned to those nodes into the underlying storage backend.

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

  [0]: http://dl.acm.org/citation.cfm?id=258660
  """
  use Behaviour
  import Enum, only: [ at: 2 ]

  @typedoc "The key used to lookup a node"
  @type nodeKey :: integer | binary

  @hash_function Application.get_env(:artifact, :hash_function)
  @hash_length 32

  ## Hash a key w/ the module & function provided in the application environment (mix.exs)
  defp crypto(hash) do
    apply(Keyword.get(@hash_function, :module), Keyword.get(@hash_function, :fun), [hash])
  end

  @doc false
  defp hash(key) do
    <<output :: @hash_length, _ :: binary>> = crypto(key)
    output
  end

  @doc false
  defp hash({{n1,n2,n3,n4}, port}, vnode) do
    <<hashed_key :: @hash_length, _rest :: binary>> =
        crypto(<< n1,n2,n3,n4, port :: 16, vnode:: 16 >>)
      hashed_key
  end

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
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

  defp bucket_members(_key_hash, _n, i, nodes) when i > 0 do
    { :nodes, Enum.reverse(nodes) }
  end
  ## List nodes whose mapped keyspace falls on this key_hash
  defp bucket_members(_key_hash, _n, i, nodes) when i == 0 do
    {:nodes, Enum.reverse(nodes)}
  end
  defp bucket_members(key_hash, n, i, nodes) do
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

  ## Fetches & writes another virtual node entry into the global vnode manifest.
  ## No locks are acquired so each manifest is node distinct .
  defp add_nodes(nodes) when nodes == [] do
    :ok
  end
  defp add_nodes([{node, info}|tail]) do
    case :ets.lookup(:node_manifest, node) do
      [ {^node, _info} | _ ] -> :ok
      [] ->
        :ets.insert(:node_manifest, {node, info})
        vnode_count = Keyword.get(:vnodes, info)
        Enum.each(1..vnode_count,
          &:ets.insert(:vnode_manifest, {hash(node, &1), node})
        )
    end
    add_nodes(tail)
  end

  @doc """
  'update_buckets' is called when a node becomes unavailable due to failures or
  maintainence, or otherwise needs to level the load handled by each individual
  machine.
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
    [node, n, buckets_count] =
      :artifact_config.get [:node, :n, :buckets_count]

    circumference    = ring_circumference(buckets_count)
    number_of_nodes  = Keyword.get(:size, :ets.info :node_manifest )

    max_search      = case number_of_nodes do
      # Don't search other nodes to fill a bucket when NumberOfNodes is
      # 1, since they are never found.
      1    -> 1
      _    -> Keyword.get(:size, :ets.info :vnode_manifest)
    end
    update_buckets(buckets_count - 1, node, circumference, n, max_search, [])
  end

  defp remove_nodes([]), do: :ok

  defp remove_nodes([node | rest]) do
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


  def update_nodes(nodes_to_add, nodes_to_remove, state) do
    local_node = :artifact_config.get(:node)
    reply =
        case {nodes_to_add, nodes_to_remove -- [local_node]} do
            {[], []} -> {:replaced_buckets, []}
            _ ->
                add_nodes(nodes_to_add)
                remove_nodes(nodes_to_remove)
                update_buckets()
        end
    {:reply, reply, state}
  end


  @doc """
  Find a bucket by either a constituent key or the bucket id
  """
  @spec locate_bucket(nodeKey(), tuple()) :: tuple()
  def locate_bucket(query, state) do
    bcount = Artifact.Config.get(:bucket_count)

    {:reply, {:bucket_count, bucket_index(query, bcount)}, state}
  end

  @doc """
  Find a replica by a constituent key or bucket
  """
  @spec locate_replica(nodeKey(), tuple()) :: tuple()
  def locate_replica(query, state) do
    node = Artifact.Config.get(node)
    {:reply, {:nodes, nodes}, state2} = locate_nodes(query, state)

    {:reply, {:replica, Enum.find_index(node, nodes)}, state2}
  end

  ## Find a node either by a constituent bucket or key
  @spec locate_nodes(nodeKey(), tuple()) :: tuple()
  defp locate_nodes(query, state) do
    bcount  = Artifact.Config.get(:bucket_count)
    bucket = bucket_index(query, bcount)
    [ { ^bucket, nodes } | _ ] = :ets.lookup(:buckets, bucket)

    {:reply, {:nodes, nodes}, state}
  end

  defp bucket_index(bucket, count) when is_integer(bucket) do
    rem(bucket, count)
  end
  defp bucket_index(key, count) do
    hash(key) / ring_circumference(count)
  end

  # A collection of utility methods for finding replicas, buckets and nodes.
  def find_bucket(key_or_bucket, state) do
    bucket_count = Artifact.Config.get(:bucket_count)
    {:reply, {:bucket, bucket_index(key_or_bucket, bucket_count)}, state }
  end

  def find_replica(key_or_bucket, state) do
    local_node = :artifact_config.get(:node)
    {:reply, {:nodes, nodes}, state2} = find_nodes(key_or_bucket, state)
    replica = Enum.find_index(nodes, &(&1==local_node))
    {:reply, {:replica, replica}, state2}
  end

  def find_nodes(key_or_bucket, state) do
    bucket_count = :artifact_config.get(:bucket_count)
    bucket       = bucket_index(key_or_bucket, bucket_count)
    [{^bucket, nodes}|_] = :ets.lookup(:buckets, bucket)
    {:reply, {:nodes, nodes}, state}
  end

  def choose_node_randomly(state) do
    {{n1, n2, n3, n4}, port} = Artifact.Config.get(:node)
    #  Build up our condition to select our nodes out of ETS.
    # TODO: Abstract this functionality out into
    head      = { '$1', '_' }
    condition = [ {'=/=', '$1', { {{{n1,n2,n3,n4}}, port} } } ]
    body      = ['$1']
    nodes     = :ets.select(:node_manifest, [{head, condition, body}])
    node_len  = length(nodes)
    case node_len do
      0 -> {:reply, :undefined, state}
      _ -> {:reply, {:node, Enum.at(nodes, :random.uniform(node_len))}, state}
    end
  end

  def choose_bucket_randomly(state) do
    local_node = :artifact_config.get(:node)
    buckets = inversed_buckets(local_node)
    len = length(buckets)
    case len do
        0 -> {:reply, :undefined, state}
        _ -> {:reply, {:bucket, at(buckets, :random.uniform(len))}, state}
    end
  end

  defp inversed_buckets(_node, -1, buckets), do: buckets
  defp inversed_buckets(node, bucket, buckets) do
    [ {bucket, nodes} | _ ] = :ets.lookup(:buckets, bucket)
    if Enum.member(node, nodes) do
      inversed_buckets(node, bucket - 1, [bucket | buckets])
    else
      inversed_buckets(node, bucket - 1, buckets)
    end
  end
  defp inversed_buckets(node), do: inversed_buckets(node, Artifact.Config.get(buckets)-1, [])

  defp do_node_info(node, state) do
    head       = {node, '$2'}
    conditions = []
    body       = ['$2']
    [info] = :ets.select(:node_manifest, [{head, conditions, body}])
    {:reply, {:node_info, node, info}, state}
  end

  defp do_node_info(state) do
    node = Artifact.Config.get(node)
    do_node_info(node, state)
  end

  def node_manifest(state) do
    node_manifest = :ets.tab2list(:node_manifest)
    mapped_list = Enum.map(node_manifest, fn({node, _info}) ->
      node
    end)
    {:reply, {:node_manifest, mapped_list}, state}
  end

  def vnode_manifest(state) do
    manifest = :ets.tab2list(:vnode_manifest)
    {:reply, {:vnode_manifest, manifest}, state}
  end

  def buckets_manifest(state) do
    buckets = :ets.tab2list(:buckets)
    {:reply, {:buckets, Enum.sort(buckets)}, state}
  end

  def buckets(state) do
    node = Artifact.Config.get(node)
    buckets = Enum.filter fn(b) ->
      Enum.member? node, at(b, 2)
      :ets.tab2list(:buckets)
    end
    buckets_new = for b <- buckets, do: at(b, 1)
    {:reply, {:buckets, Enum.sort(buckets_new)}, state}
  end

  # Callbacks

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :stopped, state}
  end
  def handle_call({:update_nodes, nodes_added, nodes_removed}, _from, state) do
    update_nodes(nodes_added, nodes_removed, state)
  end
  def handle_call({:find_bucket, key_or_bucket}, _from, state) do
    find_bucket(key_or_bucket, state)
  end
  def handle_call({:find_replica, key_or_bucket}, _from, state) do
    find_replica(key_or_bucket, state);
  end
  def handle_call({:find_nodes, key_or_bucket}, _from, state) do
    find_nodes(key_or_bucket, state)
  end
  def handle_call(:choose_node_randomly, _from, state) do
    choose_node_randomly(state)
  end
  def handle_call(:choose_bucket_randomly, _from, state) do
    choose_bucket_randomly(state)
  end
  def handle_call({:node_info, node}, _from, state), do: do_node_info(node, state)
  def handle_call(:node_info, _from, state), do: do_node_info(state)
  def handle_call(:node_manifest, _from, state), do: node_manifest(state)
  def handle_call(:vnode_manifest, _from, state), do: vnode_manifest(state)
  def handle_call(:buckets_manifest, _from, state), do: buckets_manifest(state)
  def handle_call(:buckets, _from, state), do: buckets(state)

  def handle_cast(_msg, state), do: {:noreply, state}

  def handle_info(_info, state), do: {:noreply, state}

  def code_change(_oldvsn, state, _extra), do: {:ok, state}

  def stop(), do: GenServer.call(__MODULE__, :stop)

  def update_nodes(nodes_added, nodes_removed) do
    GenServer.call(__MODULE__, {:update_nodes, nodes_added, nodes_removed})
  end

  def find_bucket(key_or_bucket) do
    GenServer.call(__MODULE__, {:find_bucket, key_or_bucket})
  end

  def find_replica(key_or_bucket) do
    GenServer.call(__MODULE__, {:find_replica, key_or_bucket})
  end

  def find_nodes(key_or_bucket) do
    GenServer.call(__MODULE__, {:find_nodes, key_or_bucket})
  end

  def choose_node_randomly() do
    GenServer.call(__MODULE__, choose_node_randomly)
  end
  def choose_bucket_randomly() do
    GenServer.call(__MODULE__, choose_bucket_randomly)
  end

  def node_info(), do: GenServer.call(__MODULE__, :node_info)
  def node_info(node), do: GenServer.call(__MODULE__, {:node_info, node})

  def node_manifest(), do: GenServer.call(__MODULE__, :node_manifest)

  def vnode_manifest(), do: GenServer.call(__MODULE__, :vnode_manifest)

  def buckets_manifest(), do: GenServer.call(__MODULE__, :buckets_manifest)

  def buckets(), do: GenServer.call(__MODULE__, :buckets)

end
