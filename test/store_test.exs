Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.Storage do
  use ExUnit.Case
  require Artifact

  alias Artifact.Config
  alias Artifact.Version
  alias Artifact.Store

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
      store: Artifact.Store.ETS,
      quorum: {1, 1, 1},
      buckets: 8,
      vnodes: 2
    )

    Version.start_link()
    Store.start_link()
    :ok
  end


  test "storage (ets)" do
    datum = Artifact.data(
      key: 'item-1',
      bucket: 3,
      last_modified: :erlang.now(),
      checksum: :erlang.md5(<<"value-1">>),
      flags: "0",
      vector_clocks: :vclock.fresh(),
      value: (<<"value-1">>)
    )

    Store.put(datum)
    # Should be able to retrieve
    assert datum == Store.get(Artifact.data(key: 'item-1', bucket: 3))

    # And should return `:undefined` if nothing exists
    assert :undefined == Store.get(Artifact.data(key: 'item-2', bucket: 1))
  end

end
