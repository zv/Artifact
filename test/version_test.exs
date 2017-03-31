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

  test "check merge_checksum (uniqueness)" do
    datum1 = Artifact.data(checksum: <<18446744073709551615::64, 0::64>>)
    datum2 = Artifact.data(checksum: <<0::64, 18446744073709551615::64>>)
    {:ok, merge} = Version.merge_clocks([datum1, datum2])
    expected = <<2::4, 15::4, 268435455::26, 0::30>>
    assert expected == merge
  end

  test "check merge_checksum(7)" do
    lst = Enum.map(1..7, fn(n) ->
      Artifact.data(checksum: <<n::8, 0::120>>)
    end)
    {:ok, merge} = Version.merge_clocks(lst)
    expected = <<7::4, 1::8, 2::8, 3::8, 4::8, 5::8, 6::8, 7::8, 0::4>>
    assert expected == merge
  end

  test "check merge_checksum(error)" do
    lst = Enum.map(1..16, fn(n) ->
      Artifact.data(checksum: <<n::4, 0::60>>)
    end)
    {:error, reason} = Version.merge_clocks(lst)
    assert String.contains? reason, "16"
  end

end
