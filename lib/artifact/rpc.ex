defmodule Artifact.RPC do
  @behaviour Artifact.TCP
  require Logger
  require Record

  alias Artifact.{Config, Hash, Store, Membership, Coordinator, Connection}

  def start_link do
    [port: port, process_limit: limit] = Config.get(:rpc)
    Artifact.TCP.Server.start_link(
      __MODULE__,
      __MODULE__,
      [],
      {[:binary, {:packet, 4}, {:active, true}, {:reuseaddr, true}],
       port: port, max_processes: limit})
  end

  def stop, do: Artifact.TCP.Server.stop(__MODULE__)
  def init(_args), do: {:ok, %{:node_info => Config.node_info()}}

  def handle_call(socket, data, state), do: dispatch("FIX NODE INFO", :erlang.binary_to_term(data), state)

  @type state :: any
  @typep socket :: state
  # @spec dispatch(state,
  #                :node_info |
  #                :node_list |
  #                { :list, node } |
  #                { :get, record(:data) } |
  #                { :delete, record(:data) } |
  #                { :check_node, node } |
  #                { :route, any } |
  #                { :put, any } |
  #                term,
  #                state) :: reply
  @doc """
  Dispatch a procedure call based on the request type encoded in the term
  """
  defp dispatch(_socket, :node_info, state), do: reply(Config.node_info(), state)
  defp dispatch(_socket, :node_list, state), do: reply(Hash.node_list(), state)
  defp dispatch(_socket, {:list, bucket}, state), do: reply(Store.list(bucket), state)
  defp dispatch(_socket, {:get, data}, state), do: reply(Store.get(data), state)
  defp dispatch(_socket, {:delete, data}, state), do: reply(Store.delete(data), state)
  defp dispatch(_socket, {:check_node, node}, state), do: reply(Membership.check_node(node), state)
  defp dispatch(_socket, {:route, req}, state), do: reply(Coordinator.route(req), state)
  defp dispatch(_socket, _, state), do: reply({:error, :enotsup}, state)
  defp dispatch(_socket, {:put, data}, state) when Record.is_record(data, :data) do
    reply(Store.put(data), state)
  end

  defp reply(data, state), do: {:reply, :erlang.term_to_binary(data), state}

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
    case Connection.lease(node, self()) do
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
        Logger.warn "request(#{inspect node}, #{inspect message}): #{inspect reason}"
        {:error, reason}
    end
  end

  defp local?(n1, n2), do: n1 == n2

  def node_info(node, src_node) do
    if local?(node, src_node) do
      Config.node_info()
    else
      request(node, :node_info)
    end
  end

  def node_list(node, src_node) do
    if local?(node, src_node) do
      Hash.node_list()
    else
      request(node, :node_list)
    end
  end

  def list(node, src_node, bucket) do
    if local?(node, src_node) do
      Store.list(bucket)
    else
      request(node, {:list, bucket})
    end
  end

  def get(node, src_node, data) do
    if local?(node, src_node) do
      Store.get(data)
    else
      request(node, {:get, data})
    end
  end

  def put(node, src_node, data) do
    if local?(node, src_node) do
      Store.put(data)
    else
      request(node, {:put, data})
    end
  end

  def delete(node, src_node, data) do
    if local?(node, src_node) do
      Store.delete(data)
    else
      request(node, {:delete, data})
    end
  end

  def check_node(node, src_node, node2) do
    if local?(node, src_node) do
      Membership.check_node(node2)
    else
      request(node, {:check_node, node2})
    end
  end

  def route(node, src_node, request) do
    if local?(node, src_node) do
       {:error, :ewouldblock}
    else
       request(node, {:route, request})
    end
  end

end
