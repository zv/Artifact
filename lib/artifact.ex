defmodule Artifact do
  use Application.Behaviour
  
  def config([], acc) do
    acc 
  end

  @doc """
    Returns Artifact's current configuration 
  """
  def config([key | rest], acc) do
    case :application.get_env(:artifact, key) do 
      {:ok, value } -> config(rest, [ {key, value} | acc ])
      :undefined     -> config(rest, acc)
    end
  end

  def start(_type, _args) do 
    args = config([
            :n, :r, :w,
            :store,
            :buckets, :tables, :vnodes,
            :rpc_processes_ceiling, :rpc_port,
            :max_connections, :interfaces
      ], [])
    :artifact_supervisor.start_link(args)
  end

  def start do
    :application.start(:artifact) 
  end

end
