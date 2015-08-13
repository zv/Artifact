defmodule Artifact.Connection do
  use GenServer

  @connect_setopts [:binary,
                    {:active, :true},
                    {:packet, 4},
                    {:reuseaddr, true}]

  @tcp_timeout 100


  def start_link do
    GenServer.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init(_args) do
    {:ok, []}
  end

  def terminate(_reason, _state) do
    :ok
  end


  @doc """
  acquire supplies the caller with a socket from a connection pool that may be
  used to issue API calls that they can later close & return.
  """
  def acquire(node, client, opts, pool) do
    case do_acquire(node, client, opts, connections, []) do
      {:ok, socket, conns} ->
        conns = lru(conns)
        {:reply, {:ok, socket}, conns}

      {:error, msg, conns} ->
        Logging.warn("acquire node: #{node}, client: #{client} with error: #{msg}")
        {:reply, {:error, msg}, conns}
    end
  end


  # These functions are the core of the connection pooling logic. We basically
  # just pop an entry off the LRU cache and go from there.
  defp do_acquire({address, port}, client, opts, [], acc) when is_pid(client) do
    # Fetch the
    case :gen_tcp.connect(address, port, @connect_setopts, @tcp_timeout) do
      {:ok, socket} ->
        case :gen_tcp.controlling_process(socket, client) do
          :ok -> push_new_connection(socket, opts, node, acc)
          {:error, msg} -> {:error, msg, acc}
        end
      {:error, msg} -> {:error, msg, acc}
    end
  end

  defp do_acquire(node, client, opts, [{node, available, socket}|t], acc) when is_pid(client) do
    case :gen_tcp.controlling_process(socket, client) do
      :ok ->
        connection_pool = {_, sock, _ } = push_new_connect(socket, opts, node, acc, rest)
        flush(sock)
        connection_pool
      {:error, msg} -> {:error, msg, acc ++ rest}
    end
  end

  defp do_acquire(node, client, opts, [c|t], acc) do
    do_acquire(node, client, opts, rest, [c|t])
  end

  defp push_new_connection(socket, opts, node, connections, rest \\ []) do
    {
      :inet.setopts(socket, opts),
      socket,
      [
        %{node: node, available: false, socket: socket} |
        Enum.reverse(connections)
      ]
    }
  end

  defp flush(socket) do
    receive do
      {:tcp, socket, _binary} -> flush(socket)
    after 0 -> ok
    end
  end

  # Fetch our least recently used entry, closing sockets that are already in use.
  defp lru(0, rest, acc), do: Enum.reverse(rest) ++ acc
  defp lru(_n, [], acc), do: acc
  defp lru(n, [c = {_, false, _}|rest], acc), do: lru(n, rest, [c|acc])
  defp lru(n, [{_, true, socket}|rest], acc) do
    :gen_tcp.close(socket)
    lru(n - 1, rest, acc)
  end
  defp lru(connections) do
    max = Artifact.Config.get(:max_connections)
    len = length(connections)
    if  len > max do
      lru(len - max, Enum.reverse(connections), [])
    else
      connections
    end
  end


  defp do_return(_, [], acc), do: {:error, :enoent, acc}
  defp do_return(socket, [{node, _, socket}|rest], acc) do
    connections = [
      %{node: node, available: avail, socket: socket} |
      Enum.reverse(connections)
    ] ++ rest

    {:ok, connections}
  end
  defp do_return(socket, [c|t], acc), do: do_return(socket, rest, [c|acc])

  def return(socket, connections) do
    case do_return(socket, connections, []) do
      {:ok, conns} -> {:reply, :ok, lru(conns)}
      {:error, msg, conns} ->
        Artifact.Logging.warn "return (#{inspect socket} error: #{inspect reason}"
    end
  end


  defp do_close(_socket, [], acc), do: {:error, :enoent, acc}
  defp do_close(socket, [c|t], acc), do: do_close(socket, rest, [c|acc])
  defp do_close(socket, [{_, _, socket}|rest], acc) do
    :gen_tcp.close(socket)
    {:ok, Enum.reverse(acc) ++ rest}
  end

  def close(socket, connections) do
    case do_close(socket, connections, []) do
      {:ok, conns} -> {:reply, :ok, conns}
      {:error, msg, conns} -> {:reply, {:error, msg}, conns}
    end
  end


  def connections(connections), do: {:reply, {:ok, connections}, connections}
  def connections(), do: :gen_server.call(__MODULE__, :connections)
  def close(socket), do: :gen_server.call(__MODULE__, {:close, socket})

  def stop(), do: :gen_server.call(__MODULE__, stop)

  def lease(node, pid) when is_pid(pid) do
    :gen_server.call(__MODULE__, {:lease, node, pid, []})
  end

  def lease(node, pid, opts) when is_pid(pid) do
    :gen_server.call(__MODULE__, {:lease, node, pid, opts})
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
        :gen_server.call(__MODULE__, {:return, socket})
      {:error, reason} ->
        Artifact.Logging.warn(:io_lib.format("return(~p) ~p", [socket, {error, reason}]))
        :gen_server.call(__MODULE__, {:close, socket})
        {:error, reason}
    end
  end

  # Callbacks

  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}
  def handle_call({:lease, node, pid, opts}, _from, state), do: lease(node, pid, opts, state)
  def handle_call({:return, socket}, _from, state), do: return(socket, state)
  def handle_call({:close, socket}, _from, state), do: close(socket, state)
  def handle_call(:connections, _from, state), do: connections(state)
  def handle_cast(_msg, state), do: {:noreply, state}
  def handle_info(_info, state), do: {:noreply, state}
  def code_change(_oldvsn, state, _extra), do: {:ok, state}

end
