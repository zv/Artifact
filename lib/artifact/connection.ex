defmodule Artifact.Connection do
  use GenServer

  @connect_setopts [:binary, {:active, :true}, {:packet, 4}, {:reuseaddr, true}]
  @tcp_timeout 5000


  def start_link, do: GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  def init(_args), do: {:ok, []}
  def terminate(_reason, _state), do: :ok


  @doc """
  Supplies the caller with a socket from a connection pool that may be
  used to issue API calls that they can later close & return.
  """
  def acquire(node, client, opts, pool) do
    case do_acquire(node, client, opts, pool, []) do
      {:ok, socket, conns} ->
        conns = lru(conns)
        {:reply, {:ok, socket}, conns}

      {:error, msg, conns} ->
        Logging.warn("acquire node: #{node}, client: #{client} with error: #{msg}")
        {:reply, {:error, msg}, conns}
    end
  end

  # These functions are the core of the connection pooling logic. We basically
  # just pop an entry off the LRU cache and lease that socket.
  defp do_acquire({address, port}, client, opts, [], acc) when is_pid(client) do
    # Fetch the
    case :gen_tcp.connect(address, port, @connect_setopts, @tcp_timeout) do
      {:ok, socket} ->
        case :gen_tcp.controlling_process(socket, client) do
          :ok ->
            :inet.setopts(socket, opts)
            conns = [
              %{node: node, available: false, socket: socket} |
              Enum.reverse(acc)
            ]
            {:ok, socket, conns}
          {:error, msg} -> {:error, msg, acc}
        end
      {:error, msg} -> {:error, msg, acc}
    end
  end

  defp do_acquire(node, client, opts, [%{node: node, available: true, socket: socket}|rest], acc)  do
    case :gen_tcp.controlling_process(socket, client) do
      :ok ->
        :inet.setopts(socket, opts)
        conns = [
          %{node: node, available: false, socket: socket} |
          Enum.reverse(acc)
        ] ++ rest
        flush(socket)
        {:ok, socket, conns}
      {:error, msg} -> {:error, msg, acc ++ rest}
    end
  end

  defp do_acquire(node, client, opts, [c|rest], acc) do
    do_acquire(node, client, opts, rest, [c|acc])
  end

  defp flush(socket) do
    receive do
      {:tcp, socket, _binary} -> flush(socket)
    after 0 -> :ok
    end
  end

  # Fetch our least recently used entry, closing sockets that are already in use.
  defp lru(0, rest, acc), do: Enum.reverse(rest) ++ acc
  defp lru(_n, [], acc), do: acc
  defp lru(n, [%{node: _, available: true, socket: socket}|rest], acc) do
    :gen_tcp.close(socket)
    lru(n - 1, rest, acc)
  end
  defp lru(n, [c = %{node: _, available: false, socket: _}|rest], acc), do: lru(n, rest, [c|acc])
  defp lru(connections) do
    max = Artifact.Config.get(:max_connections)
    len = length(connections)
    if len > max do
      lru(len - max, Enum.reverse(connections), [])
    else
      connections
    end
  end

  defp do_return(_, [], acc), do: {:error, :enoent, acc}
  defp do_return(socket, [%{node: node, available: _avail, socket: socket} | rest], acc) do
    connections = [
      %{node: node, available: true, socket: socket} |
      Enum.reverse(acc)
    ] ++ rest

    {:ok, connections}
  end
  defp do_return(socket, [c|t], acc), do: do_return(socket, t, [c|acc])

  def return(socket, connections) do
    case do_return(socket, connections, []) do
      {:ok, conns} -> {:reply, :ok, lru(conns)}
      {:error, msg, conns} ->
        # TODO: Logging.warn
        IO.puts "return/2 (#{inspect socket} error: #{inspect msg}"
        {:reply, {:error, msg}, conns}
    end
  end


  defp do_close(_socket, [], acc), do: {:error, :enoent, acc}
  defp do_close(socket, [%{node: _, available: _, socket: socket}|rest], acc) do
    :gen_tcp.close(socket)
    {:ok, Enum.reverse(acc) ++ rest}
  end
  defp do_close(socket, [c|t], acc), do: do_close(socket, t, [c|acc])

  def close(socket, connections) do
    case do_close(socket, connections, []) do
      {:ok, conns} -> {:reply, :ok, conns}
      {:error, msg, conns} -> {:reply, {:error, msg}, conns}
    end
  end


  def connections(conns), do: {:reply, {:ok, conns}, conns}
  def connections(), do: GenServer.call(__MODULE__, :connections)

  def close(socket), do: GenServer.call(__MODULE__, {:close, socket})

  def stop(), do: GenServer.call(__MODULE__, :stop)

  def acquire(node, pid) when is_pid(pid) do
    GenServer.call(__MODULE__, {:acquire, node, pid, []})
  end

  def acquire(node, pid, opts) when is_pid(pid) do
    GenServer.call(__MODULE__, {:acquire, node, pid, opts})
  end

  defp reset_controlling_process(socket) do
    case Process.whereis(__MODULE__) do
      pid when is_pid(pid) -> :gen_tcp.controlling_process(socket, pid);
      _                    -> {:error, :esrch} # like *that* will ever happen....
    end
  end

  def return(socket) when is_port(socket) do
    case reset_controlling_process(socket) do
      :ok ->
        GenServer.call(__MODULE__, {:return, socket})
      {:error, msg} ->
        # TODO FUQED
        # Artifact.Logging.warn(:io_lib.format("return(~p) ~p", [socket, {error, reason}]))
        GenServer.call(__MODULE__, {:close, socket})
        {:error, msg}
    end
  end

  # Callbacks

  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}
  def handle_call({:acquire, node, pid, opts}, _from, state), do: acquire(node, pid, opts, state)
  def handle_call({:return, socket}, _from, state), do: return(socket, state)
  def handle_call({:close, socket}, _from, state), do: close(socket, state)
  def handle_call(:connections, _from, state), do: connections(state)
  def handle_cast(_msg, state), do: {:noreply, state}
  def handle_info(_info, state), do: {:noreply, state}
  def code_change(_oldvsn, state, _extra), do: {:ok, state}

end
