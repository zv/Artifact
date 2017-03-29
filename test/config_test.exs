Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.Config do
  use ExUnit.Case

  setup_all do
    {:ok, _config} = Artifact.Config.start_link(
      [hostname: 'localhost',
       participants: 2,
       buckets: 2048,
       vnodes: 256,
       quorum: {1,1,1},
       rpc: [port: 10070]]
    )

    # on_exit fn ->
    #   # This could be fucked
    #   # Artifact.Config.stop()
    # end

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

  test "Correctly handle quorum" do
    assert Artifact.Config.get([:n, :r, :w]) == [1,1,1]
  end

end
