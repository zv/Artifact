* Artifact
  Artifact is a distributed key-value datastore, which is inspired
  by the [[http://www.cs.ucsb.edu/~agrawal/fall2009/dynamo.pdf][Amazon's Dynamo Paper]].

  I've benchmarked artifact at 1,200 querys-per-second for a 16 node system with
  1GB assigned to each container (single machine). At peak load, 99% of requests
  fell under 300ms.
 
** Features
   + Consistently hashing load distribution
   + Fair Load balancing
   + Secondary Indexes
   + Custom storage backends (ETS, DETS are standard)
   + Quorum coordinator
   + Memcache and TLL interfaces
   + Gossip protocol membership

** Warning!
  This is an Elixir rewrite of my original Erlang project and is *not* guaranteed to work the same way.

  This database could have major problems, I've never submitted it to any sort
  of substantial peer review or audit. I've been working on it for some time and
  I'm still hashing things out. If you find any such problems please file an issue!

* Running 
** Standalone
   The easiest way to get up and running with Artifact is to boot a standalone
   instance. Artifacts configuration (=config/config.exs=) is configured for this
   use-case by default.

** Clustering
   #+BEGIN_EXAMPLE
   iex(1)> Application.start(:artifact)
   :ok
   iex(2)> Artifact.RPC.check('kai2.local': 11011, 'kai1.local': 11012)
   :ok
   iex(3)> Artifact.RPC.check('kai2.local': 11011, 'kai1.local': 11012)
   :ok
   #+END_EXAMPLE
   
   Artifat maintains a node manifest, including the members of it's own cluster.
   When invoking =check=, it's asking to retrieve the manifest manually from
   another node. Calling this function twice can bootstrap the process by 
   mutually ensuring that each node has been added to the other's manifest.
   
   Synchronization will need to synchronize =log2(STORAGE_CNT)*32= + overhead
   bytes, so give it some time if you've built up a large entry count.
   
   To verify that a node has been added to the node manifest:

   #+BEGIN_EXAMPLE
   iex(5)> Artifact.RPC.node_manifest('kai1.local')
   {:node_manifest, [{{'10.0.0.53', 11011}}, {{'10.0.0.80', 11011}}]
   #+END_EXAMPLE

** Memcache
   Once you have a server started, you can read and write to it just as if it
   were a memcached daemon.

   #+BEGIN_EXAMPLE
   % nc localhost 11211
   set foo 0 0 3
   bar
   STORED
   get foo
   VALUE foo 0 3
   bar
   #+END_EXAMPLE

* Configuration
  All configuration values are loaded at startup from =config/config.exs=. If you'd like to know what configuration
  a system is running, you can query it with

  #+BEGIN_EXAMPLE
  iex> Artifact.config.node_info
  {:node_info, {{127,0,0,1}, 10070}, [{:vnodes, 256}]}
  #+END_EXAMPLE

** Quorum
    #+BEGIN_SRC elixir
quorum: {
  # Number of Participants
  1,
  # Reader workers
  1,
  # Writer workers
  1
},
    #+END_SRC
    Artifact doesn't keep a strict quorum system and so the values used in
    quorum refer to the minimum number of nodes to participate in some operation
    for it to be successful. =N=, the first parameter, is the number of
    participants in the 'preference list' and although there can be more nodes
    added to the system than =N=, those excess nodes won't verify replicas. =R=,
    the second, is the minimum number of nodes that must participate in a
    successful read operation. =W= is the minimum number of nodes that must
    participate in a successful write operation. The latency of a get (or put)
    operation is dictated by the slowest of the =R= (or =W=) replicas. For this
    reason, =R= and =W= are usually configured to be less than N, to provide
    better latency.

** Ring Parameters & Load Balancing
    #+BEGIN_SRC elixir
# Buckets are the atomic unit of interdatabase partitioning. Adjusting
# this upward will give you more flexibility in how finely grained your
# data will be split at the cost of more expensive negotiation.
buckets: 2048,

# Individual physical nodes present themselves as a multitude of
# virtual nodes in order to address the problem within consistent
# hashing of a non-uniform data distribution scheme, therefore vnodes
# represents a limit of how many physical nodes exist in your
# datastore.
vnodes: 256,
tables: 512,
    #+END_SRC

    Instead of mapping nodes to a single point in the ring, each node gets
    assigned to multiple points. To this end, Artifact uses the Dynamo concept
    of "virtual nodes". A virtual node looks like a single node in the system,
    but each node can be responsible for more than one virtual node.
    Effectively, when a new node is added to the system, it is assigned multiple
    positions ("tokens") in the ring. Using virtual nodes has the following
    advantages:

    - If a node becomes unavailable (due to failures or routine maintenance), the load handled by this node is evenly dispersed across the remaining available nodes.
    - When a node becomes available again, or a new node is added to the system, the newly available node accepts a roughly equivalent amount of load from each of the other available nodes.
    - The number of virtual nodes that a node is responsible can decided based on its capacity, accounting for heterogeneity in the physical infrastructure.

** Storage
    #+BEGIN_SRC elixir
store: Artifact.Store.ETS,
    #+END_SRC
    
    I haven't ported DETS over from my original Erlang implementation yet and so
    you are stuck with ETS for now.
