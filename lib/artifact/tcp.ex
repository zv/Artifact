defmodule Artifact.TCP do
  @callback init(arg :: any) :: any
  @callback handle_call(arg :: any, arg1 :: any, arg2 :: any) :: any

  def start_link(mod), do: start_link(mod, [])
  def start_link(mod, args), do: start_link(mod, args, Application.get_env(:artifact, TCP))
  def start_link(mod, args, options), do: start_link(__MODULE__, mod, args, options)
  def start_link(name, mod, args, options) do
    Artifact.TCP.Supervisor.start_link(name, mod, args, options)
  end

  def stop, do: stop(__MODULE__)
  def stop(name), do: Artifact.TCP.Supervisor.stop(name)

  def info(key), do: info(__MODULE__, key)
  def info(name, key) do
    Artifact.TCP.Supervisor.build_monitor_name(name) |> TCP.Monitor.info(key)
  end

end
