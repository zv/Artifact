Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.Test do
  use ExUnit.Case
  alias Artifact.Config
  alias Artifact.Hash
  import Artifact.TestMacros, only: :macros

  # setup_all do
  #   # IO.puts "Initializing Config Server..."
  #   # {:ok, config} = Config.start_link(
  #   #   [hostname: 'localhost',
  #   #    participants: 2,
  #   #    buckets: 2048,
  #   #    vnodes: 256,
  #   #    rpc: [port: 10070]]
  #   # )
  #   # {:ok, connection} = Connection.start_link()
  #   # spawn_link(echo_start)
  #   # {:ok, connection: connection }
  #   # { :ok, config: config }
  # end


  test "hashing" do
    Config.start_link(
      hostname: 'localhost',
      rpc: [port: 11011],
      quorum: {1, 1, 1},
      buckets: 8,
      vnodes: 2
    )

    Hash.start_link()

    expected_info = [{:number_of_virtual_nodes, 2}]
    IO.puts "#{inspect Hash.node_info()}"
    # {:node_info, node1, info} = Hash.node_info()
    # assert info == expected_info

    # {:node_info, node1, ^info} = Hash.node_info(node1)
    # assert info == expected_info
  end

end
