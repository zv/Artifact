defmodule Artifact.Hash do
  @compile {:nowarn_unused_function, [derive_node_manifest: 4]}
  @on_load :register_hash_function
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
  # Defines the prefix length of our hash
  defmacrop hash_length, do: 32

  defmacrop register_hash_function do
    case :application.get_env(:hash_function) do
      :md5 -> 
          def hash(key) do
            <<output :: size(32), _ :: binary>> = :erlang.md5(key)
            output 
          end

          def hash({{n1,n2,n3,n4}, port}, vnode) do 
            << hashed_key :: [size(hash_length), integer] , _rest :: binary >> = 
                :erlang.md5(<< n1,n2,n3,n4, port :: 16, vnode:: 16 >>)
              hashed_key
          end

          :ok # We have to return ok in response to @on_load

      _ -> nil 
    end
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
  Searches the nodes by buckets to produce a list of nodes given
  participants and the hashed key.
  """
  defp derive_node_manifest(_key_hash, _n, i, nodes) when i == 0 do
    {:nodes, Enum.reverse(nodes)}
  end
  defp derive_node_manifest(key_hash, n, i, nodes) do
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
      _   -> derive_node_manifest(key_hash, n, i-1, new_nodes)
    end
  end

  # Identifies the range of buckets
  def ring_circumference(bucket_count) do
    trunc(:math.pow(2, hash_length) / bucket_count)
  end

  @doc """
    Fetches & writes another virtual node entry into the global vnode manifest.
    No locks are acquired so each manifest is node distinct .
  """
  def add_nodes(nodes) when nodes == [] do
    :ok
  end
  def add_nodes([{node, info}|tail]) do
    case ets:lookup(:node_list, node) do
      ^[ {node, _info} | _ ] -> :ok
      [] ->
        ets:insert(:node_list, {node, info}),
          vnode_count = Keyword.get(:vnodes, info)
          Enum.each(1..vnode_count, fn(vnode) ->
            key = hash(node, vnode)
            :ets.insert(:vnode_manifest, {key, node})
          end
    end
    add_nodes(tail)
  end


  def remove_nodes([]), do: :ok

  def remove_nodes([head | tail]) do
    case :ets.lookup(:node_manifest, node) do 
      [{node, info}|_] ->
          :ets.delete(:node_manifest, node)
          vnode_count = Keyword.get(:vnodes, info)
          Enum.each 1..vnode_count, fn(vnode) -> 
            :ets.delete(:vnodes, hash(node, vnode)) 
          end
      [] -> :ok
    end
    remove_nodes(tail)
  end

end
