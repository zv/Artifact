Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.Config do
  use ExUnit.Case

  setup_all do
    IO.puts "Initializing Config Server..."
    Artifact.Config.start_link([
      hostname: 'localhost',
      participants: 2,
      buckets: 2048,
      vnodes: 256,
      rpc: [
        port: 10070
      ]
    ])
    { :ok, from_setup: :hello }
  end

  teardown_all do
    Artifact.Config.stop()
    :ok
  end

  test "got Node information config" do
    assert Artifact.Config.get(:node) == {{127,0,0,1}, 10070}
  end

  test "Got node info" do
    assert Artifact.Config.node_info ==
      {:node_info, {{127,0,0,1}, 10070}, [{:vnodes, 256}]}
  end

  test "got Bucket count config" do
    assert Artifact.Config.get(:buckets) == 2048
  end

  test "got Virtual Node count config" do
    assert Artifact.Config.get(:vnodes) == 256
  end

  test "Got multipart configuration" do
    assert Artifact.Config.get([:node, :buckets, :vnodes]) ==
      [{{127,0,0,1}, 10070}, 2048, 256]
  end

end
