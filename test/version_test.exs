Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.Version do
  use ExUnit.Case
  require Artifact
  alias Artifact.Config
  alias Artifact.Version
  alias Artifact.Store

  import Artifact.TestMacros, only: :macros

  setup do
    Config.start_link(
      hostname: 'localhost',
      rpc: [
        port: 11011,
        process_limit: 2
      ],
      interfaces: [
        max_connections: 32,
        memcache: [
          port: 11211,
          process_limit: 2
        ]
      ],
      quorum: {1, 1, 1},
      buckets: 8,
      vnodes: 2
    )

    Version.start_link()

    # on_exit fn() ->
    #   Config.stop()
    #   Version.stop()
    # end

    :ok
  end

  test "vclock correctly descends" do
    vclock = :vclock.increment(Config.get(:node), :vclock.fresh())
    datum = Artifact.data(vector_clocks: vclock)

    {:ok, datum2} = Version.update(datum)
    IO.puts("datum.vector_clocks: #{inspect Artifact.data(datum2, :vector_clocks)}")

    assert :erlang.is_list(Artifact.data(datum2, :vector_clocks))

    assert :vclock.descends(Artifact.data(datum2, :vector_clocks),
                            Artifact.data(datum, :vector_clocks))

    assert !(:vclock.descends(Artifact.data(datum, :vector_clocks),
                              Artifact.data(datum2, :vector_clocks)))
  end

  defp set_bits(n), do: List.duplicate(255, n)
  defp cleared_bits(n), do: List.duplicate(0, n)

  test "check merge_checksum (packing)" do
    datum = Artifact.data(checksum: :erlang.list_to_binary(set_bits(16)))
    {:ok, checksum} = Version.merge_clocks([datum])
    expected = :erlang.list_to_binary([<<1::4, 15::4>>, set_bits(7)])
    assert expected == checksum
  end

end
