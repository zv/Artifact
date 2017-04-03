defmodule Artifact.RPC do
  import Artifact, only: [server_options: 1]
  alias Artifact.Config
  alias Artifact.Connection
  alias Artifact.Store

  def start_link() do
    [port: port, process_limit: limit] = Config.get(:rpc)
    options = Artifact.server_options([
      listen: [:binary,
               {:packet, 4},
               {:active, true},
               {:reuseaddr, true}],
      port: port,
      max_processes: limit,
      shutdown: 2000
    ])

    opts = [port: port]

    {:ok, _} = :ranch.start_listener(
      :artifact,             # Application
      limit,                 # Number of spawned acceptors
      :ranch_tcp,            # Type
      opts,                  # Socket options
      Artifact.RPC.Acceptor, # Pool module
      [])                    # Arguments
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

  def node_info(node) do
    if local?(node) do
      Config.node_info()
    else
      request(node, :node_info)
    end
  end

  def node_list(node) do
    if local?(node) do
      Hash.node_list()
    else
      request(node, :node_list)
    end
  end

  def list(node, bucket) do
    if local?(node) do
      Store.list(bucket)
    else
      request(node, {:list, bucket})
    end
  end

  def get(node, data) do
    if local?(node) do
      Store.get(data)
    else
      request(node, {:get, data})
    end
  end

  def put(node, data) do
    if local?(node) do
      Store.put(data)
    else
      request(node, {:put, data})
    end
  end

  def delete(node, data) do
    if local?(node) do
      Store.delete(data)
    else
      request(node, {:delete, data})
    end
  end

  def check_node(node, node2) do
    if local?(node) do
      Membership.check_node(node2)
    else
      request(node, {:check_node, node2})
    end
  end

  def route(node, request) do
    if local?(node) do
       {:error, :ewouldblock}
    else
       request(node, {:route, request})
    end
  end

end

defmodule Artifact.RPC.Acceptor do
  require Record
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

  defp dispatch({:get, datum}, state), do: reply(Artifact.Store.get(datum), state)
  defp dispatch(:node_info, state), do: reply(Config.node_info(), state)
  defp dispatch(:node_list, state), do: reply(Hash.node_list(), state)
  defp dispatch({:list, bucket}, state), do: reply(Store.list(bucket), state)
  defp dispatch({:get, datum}, state), do: reply(Store.get(datum), state)
  defp dispatch({:delete, datum}, state), do: reply(Store.delete(datum), state)
  defp dispatch({:check_node, node}, state), do: reply(Membership.check_node(node), state)
  defp dispatch({:route, req}, state), do: reply(Coordinator.route(req), state)
  defp dispatch({:put, datum}, state) when Record.is_record(datum, :datum) do
    reply(Store.put(datum), state)
  end
  defp dispatch(_, state), do: reply({:error, :enotsup}, state)

end
