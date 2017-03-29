Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.Test do
  use ExUnit.Case
  alias Artifact.Config
  alias Artifact.Hash
  import Artifact.TestMacros, only: :macros

  setup_all do
    Config.start_link(
      hostname: 'localhost',
      rpc: [port: 11011],
      quorum: {3, 3, 3},
      buckets: 8,
      vnodes: 2,
      hash_function: [
        module: :'erlang',
        fun: :'md5',
      ]
    )

    Hash.start_link()

    :ok
  end

  test "node_info" do
    expected_info = [{:vnodes, 2}]
    {:node_info, node1, info} = Hash.node_info()
    assert info == expected_info
    {:node_info, node1, ^info} = Hash.node_info(node1)
    assert info == expected_info
  end

  test "virtual node_info" do
    {:vnode_manifest, vnode_manifest} = Hash.vnode_manifest()
    assert [{2311136591, {{127, 0, 0, 1}, 11011}},
            {3495790055, {{127, 0, 0, 1}, 11011}}] == vnode_manifest
  end

  test "bucket check" do
    {:buckets_manifest, bucket_manifest} = Hash.buckets_manifest()
    assert [{0, [node1]},
            {1, [node1]},
            {2, [node1]},
            {3, [node1]},
            {4, [node1]},
            {5, [node1]},
            {6, [node1]},
            {7, [node1]}] == bucket_manifest
  #end

  #test "bucket hash" do
    {:buckets, buckets} = Hash.buckets()
    assert [0, 1, 2, 3, 4, 5, 6, 7] == buckets
  #end

  #test "replace nodes" do
    expected_info = [{:vnodes, 2}]
    {:reorganized_buckets, replaced} = Hash.update_nodes(
      [{node2, expected_info}, {node3, expected_info}, {node4, expected_info}],
      []
    )

    assert replaced == [{0, 2, 0}, {1, 2, 0}, {2, 2, 0}, {3, 1, 0}, {4, 1, 0}, {5, 2, 0}, {7, 2, 0}]
  #end

  #test "node list" do
    {:node_manifest, manifest} = Hash.node_manifest()
    assert length(manifest) == 4
    assert Enum.member?(manifest, node2())
    assert Enum.member?(manifest, node3())
    assert Enum.member?(manifest, node4())
  end



end
