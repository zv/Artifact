defmodule Artifact.Supervisor do
  import Supervisor.Spec

  def start_link(_args), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [
      worker(Artifact.Config, [[name: Config]]),
      worker(Artifact.Logging, [[name: Logging]]),
      worker(Artifact.Hash, [[name: Hash]]),
      worker(Artifact.Store, [[name: Store]]),
      worker(Artifact.State, [[name: Stat]]),
      worker(Artifact.Connection, [[name: Connection]]),
      worker(Artifact.Sync, [[name: Sync]]),
      worker(Artifact.Membership, [[name: Membership]]),
      worker(Artifact.RPC, [[name: RPC]]),
      worker(Artifact.Memcache, [[name: Memcache]])
    ]
    supervise(tree, strategy: :one_for_one)
  end
end
