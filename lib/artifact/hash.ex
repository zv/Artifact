defmodule Artifact.Hash do
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
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], _options = [])
  end

  def init(_args) do
    :ets.new :buckets        , [:set         , :private , :named_table]
    :ets.new :node_manifest  , [:set         , :private , :named_table]
    :ets.new :vnode_manifest , [:ordered_set , :private , :named_table]
    { :ok, [] }
  end
end
