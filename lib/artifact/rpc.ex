defmodule Artifact.RPC do
  alias Artifact.Config
  alias Artifact.Connection
  alias Artifact.Store

  def start_link() do
    [port: port, process_limit: limit] = Config.get(:rpc)
    options = Artifact.TCP.server_options([
      listen: [:binary,
               {:packet, 4},
               {:active, true},
               {:reuseaddr, true}],
      port: port,
      max_processes: limit,
      shutdown: 2000
    ])

    opts = [port: port]

    {:ok, _} = :ranch.start_listener(:artifact, 100, :ranch_tcp, opts, Artifact.RPC.Acceptor, [])
  end



  defp recv_response(socket) do
    receive do
      {:tcp, ^socket, bin}   -> {:ok, :erlang.binary_to_term(bin)}
      {:tcp_closed, ^socket} -> {:error, :econnreset}
      {:error, reason}       -> {:error, reason}
    after
      @timeout -> {:error, :timeout}
    end
  end

  defp do_request(node, message) do
    case Connection.acquire(node, self()) do
      {:ok, socket} ->
        case :gen_tcp.send(socket, :erlang.term_to_binary(message)) do
          :ok -> case recv_response(socket) do
                   {:ok, result} ->
                     Connection.return(socket)
                     {:ok, result}
                   {:error, reason} ->
                     Connection.close(socket)
                     {:error, reason}
                 end
          {:error, reason} ->
            Connection.close(socket)
            {:error, reason}
        end
      {:error, reason} -> {:error, reason}
    end
  end


  defp request(node, message) do
    case do_request(node, message) do
      {:ok, result}    -> result
      {:error, reason} ->
      # Logger.warn "request(#{inspect node}, #{inspect message}): #{inspect reason}"
      {:error, reason}
    end
  end


  defp local?(node), do: node == Config.get(:node)

  def get(node, data) do
    case local?(node) do
      true -> Store.get(data)
      _    -> request(node, {:get, data})
    end
  end


end

defmodule Artifact.RPC.Acceptor do
  alias Artifact.{Config, Hash, Store, Membership, Coordinator, Connection}

  def start_link(ref, socket, transport, opts) do
    pid = spawn_link(__MODULE__, :init, [ref, socket, transport, opts])
    {:ok, pid}
  end

  def init(ref, socket, transport, opts) do
    :ok = :ranch.accept_ack(ref)

    IO.puts("RPC client connected")

    state = %{
      socket: socket,
      transport: transport,
      server_opts: opts
    }

    listen(state)
  end

  def listen(state) do
    case recv(state) do
      {:reply, to_send, state} ->
        IO.puts("got back response")
        state.transport.send(to_send)
        listen(state)
      {:noreply, state} -> listen(state)
      :closed ->
        IO.puts("socket closed")
        :ok
    end
  end

  def recv(state) do
    case state.transport.recv(state.socket,
          state.opts.recv_length,
          state.opts.recv_timeout) do
      {:ok, header} ->
        do_dispatch(header, state)
      _ ->
        :ok = state.transport.close(state.socket)
        :closed
    end
  end

  defp reply(datum, state), do: {:reply,
                                 {:erlang.term_to_binary(datum), state}}

  defp do_dispatch(payload, state) do
    dispatch(state, :erlang.binary_to_term(payload))
  end

  def dispatch({:get, datum}, state) do
    reply(
      Artifact.Store.get(datum), state
    )
  end

end
