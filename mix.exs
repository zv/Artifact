defmodule Artifact.Mixfile do
  use Mix.Project

  def project do
    [ app: :artifact,
      version: "0.1",
      elixir: "~> 0.15.0",
      deps: deps ]
  end

  def application do
    [
      env: [
        # Basic process configuration
        rpc: [
          port: 11011,
          process_limit: 120
        ],

        # Number of Participants
        n: 1,
        # Reader workers
        r: 1,
        # Writer workers
        w: 1,

        # Buckets are the atomic unit of interdatabase partitioning. Adjusting
        # this upward will give you more flexibility in how finely grained your
        # data will be split at the cost of more expensive negotiation.
        bucket_count: 2048,

        # Individual physical nodes present themselves as a multitude of
        # virtual nodes in order to address the problem within consistent
        # hashing of a non-uniform data distribution scheme, therefore vnodes
        # represents a limit of how many physical nodes exist in your
        # datastore.
        vnode_count: 256,
        tables_count: 512,

        # Only ETS is a valid storage backend at this point
        store: "ets",

        # Only Memcache is a valid interface mechanism at this point
        interfaces: [
          max_connections: 128,
          memcache: [
            port: 11211,
            process_limit: 10,
          ]
        ],

        # hash_function provides the module and name of the hash function whose
        # output of a piece of data's signature is used to determine it's
        # position on the 'hash ring', a circular structure mapping the valid
        # possible outputs of a hash function to individual nodes. MD5 is used
        # for testing, Murmur hashing should be used otherwise.
        hash_function: [
          module: "erlang", fun: "md5"
          # module: "ghelper", fun: "murmur32"
        ],

      ]
    ]
  end

  defp deps do
  end
end
