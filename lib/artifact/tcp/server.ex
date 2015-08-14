defmodule Artifact.TCP.Server do
  #TODO: This is totally incorrect, {:node_info} should contain something to the
  #effect of the return of Config.node_info
  @callback init(args :: atom) :: { atom, {:node_info} }
  @callback handle_call(socket :: :inet.socket, data :: binary, state :: {:node_info} ) :: any

  # External APIs
  def start_link(module), do: start_link(module, [])
  def start_link(module, args), do: start_link(module, args, Config.tcp_options)
  def start_link(module, args, option) do
    start_link({:local, __MODULE__}, module, args, option)
  end
  def start_link(name, module, args, option) do
    TCP.Supervisor.start_link(name, module, args, option)
  end

  def stop, do: stop(__MODULE__)
  def stop(module), do: TCP.Supervisor.stop(module)

  def info(key), do: info(__MODULE__, key)
  def info(name, key) do
    TCP.Monitor.info(TCP.Supervisor.build_monitor_name(name), key)
  end
end
