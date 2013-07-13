defmodule Artifact.Sup do
  use Supervisor.Behaviour

  # The __MODULE__ option is analagous to ?MODULE in erlang
  defmacro server do
    __MODULE__
  end

  # Callbacks

  def start_link(args) do 
    :supervisor.start_link({:local, server}, server, args)
  end

  def init(user_options) do
    config = worker(Artifact.Config, [user_options])
    tree = [config]
    supervise(tree, strategy: :one_for_one) 
  end
end
