use Mix.Config

config :artifact,
  logfile: "artifact.log",

  # Basic process configuration
  rpc: [
    port: 11011,
    process_limit: 120
  ],

  quorum: {
    # Number of Participants
    1,
    # Reader workers
    1,
    # Writer workers
    1
  },

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

  # Only ETS is a valid storage backend at this point
  store: Artifact.Store.ETS,

  # Only Memcache is a valid interface mechanism at this point
  interfaces: [
    max_connections: 128,
    memcache: [
      port: 11211,
      process_limit: 10,
    ]
  ],

  timer: 1000,

  version: "Artifact/0.3 [elixir]",

  # hash_function provides the module and name of the hash function whose
  # output of a piece of data's signature is used to determine it's
  # position on the 'hash ring', a circular structure mapping the valid
  # possible outputs of a hash function to individual nodes. MD5 is used
  # for testing, Murmur hashing should be used otherwise.
  hash_function: [
    module: :'erlang',
    fun: :'md5',
    # module: "ghelper",
    # fun: "murmur32"
  ]


config :artifact, TCP,
  listen: [
    {:active, false},
    :binary,
    {:packet, :line},
    {:reuseaddr, true}
  ],
  port: 11211,
  max_processes: 8,
  max_restarts: 3,
  time: 60,
  shutdown: 2000,
  accept_timeout: :infinity,
  accept_error_sleep_time: 3000,
  recv_length: 0,
  recv_timeout: :infinity


config :logger,
  level: :debug,
  truncate: 4096
