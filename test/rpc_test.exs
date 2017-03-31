Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.RPC do
  use ExUnit.Case
  alias Artifact.Config
  alias Artifact.Hash
  alias Artifact.Store
  alias Artifact.Connection
  alias Artifact.RPC

  import Artifact.TestMacros, only: :macros

  setup_all do
    Config.start_link(
      hostname: 'localhost',
      rpc: [
        port: 11011,
        process_limit: 2
      ],
      interfaces: [
        max_connections: 32
      ],
      quorum: {3, 3, 3},
      buckets: 8,
      vnodes: 2,
      store: Artifact.Store.ETS
    )

    Hash.start_link()
    Store.start_link()
    Connection.start_link()
    Artifact.RPC.start_link()

    # Wait for RPC to start
    :timer.sleep(100)

    :ok
  end

  test "global rpc_test" do
    thing = RPC.node_info(node1())
    IO.puts(inspect(thing))
  end

end
