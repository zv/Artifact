defmodule Artifact.Supervisor do
  import Supervisor.Spec

  def start_link(), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [
      worker(Artifact.Config, [Application.get_all_env(:artifact)]),
      worker(Artifact.Logging, []),
      worker(Artifact.Hash, []),
      worker(Artifact.Store, []),
      worker(Artifact.Stat, []),
      worker(Artifact.Connection, []),
      worker(Artifact.Sync, []),
      worker(Artifact.Membership, []),
      worker(Artifact.RPC, []),
      worker(Artifact.Memcache, [[name: Memcache]])
    ]
    supervise(tree, strategy: :one_for_all)
  end
end
