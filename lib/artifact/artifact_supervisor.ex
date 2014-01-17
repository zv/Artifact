defmodule Artifact.Supervisor do
  use Supervisor

  # Callbacks

  def start_link(args), do: Supervisor.start_link(__MODULE__, :ok)

  @logger Artifact.Logging
  @config Artifact.Config

  def init(:ok) do
    tree = [
      worker(Artifact.Config, [[name: @config]]),
      worker(Artifact.Logging, [[name: @logger]])
    ]
    supervise(tree, strategy: :one_for_one) 
  end
end
