defmodule Artifact.TCP.Acceptor do
  use GenServer

  def start_link({:global, name}, socket, state, monitor_name, module, option) do
    {:ok, pid} = :proc_lib.start_link(
      __MODULE__, :init,
      [self(), socket, state, monitor_name, module, option]
    )
    :global.register_name(name, pid)
    {:ok, pid}
  end

  def start_link(name, socket, state, monitor_name, module, option) do
    result = :proc_lib.start_link(
      __MODULE__,
      :init,
      [self(), socket, state, monitor_name, module, option])
    #:erlang.register(name, pid)
    #Process.register(pid, name)
    {:ok, pid}
  end

  def init(parent, socket, state, monitor_name, module, option) do
    #:proc_lib.init_ack(parent, {:ok, self()})
    Artifact.TCP.Monitor.register(monitor_name, self())
    accept(socket, state, monitor_name, module, option)
  end

  def accept(socket, state, monitor_name, module, option) do
    case :gen_tcp.accept(socket, option.accept_timeout) do
      {:ok, listen_socket} ->

        Artifact.TCP.Monitor.increment(monitor_name, self)
        recv(option[:listen][:active],
          listen_socket, state, module, option)
        Artifact.TCP.Monitor.decrement(monitor_name, self)
        :gen_tcp.close(listen_socket)
      {:error, msg} ->
        # Artifact.Logging.warning("accept #{inspect module} msg: #{inspect msg}")
        :timer.sleep(option.accept_error_sleep_time)
    end

    accept(socket, state, monitor_name, module, option)
  end


  defp recv(false, socket, state, mod, option) do
    case(:gen_tcp.recv(socket, option[:recv_length], option[:recv_timeout])) do
      {:ok, data}       -> call_mod(false, socket, data, state, mod, option)
      {:error, :closed} -> :tcp_closed
      {:error, reason} ->
        # Artifact.logging("recv(#{inspect mod}) #{inspect reason}")
        :error
    end
  end

  defp recv(true, _socket, state, mod, option) do
    receive() do
      {:tcp, socket, data} -> call_mod(true, socket, data, state, mod, option)
      {:tcp_closed, _socket} -> :tcp_closed
      {:error, msg} ->
        # Artifact.logging("recv(#{inspect mod}) #{inspect msg}")
        :error
    after option[:recv_timeout] -> :tcp_timeout
    end
  end

  defp call_mod(active, socket, data, state, mod, option) do
    case(mod.handle_call(socket, data, state)) do
      {:reply, data_to_send, ^state} ->
        :gen_tcp.send(socket, data_to_send)
        recv(active, socket, state, mod, option)
      {:noreply, ^state}             -> recv(active, socket, state, mod, option)
      {:close, ^state}               -> :tcp_closed
      {:close, data_to_send, ^state} -> :gen_tcp.send(socket, data_to_send)
      other                          -> # Artifact.logging("call_mod(#{mod}) #{other}")
        IO.puts("call_mod(#{mod}) #{other}")
    end
  end
end
