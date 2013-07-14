defmodule Artifact.Supervisor do
  use Supervisor.Behaviour

  defmacro server do
    __MODULE__
  end

  # Callbacks

  def start_link(args) do 
    :supervisor.start_link({:local, server}, server, args)
  end

  def init(user_options) do
    tree = [
      worker(Artifact.Config, [user_options])
    ]
    supervise(tree, strategy: :one_for_one) 
  end
end
