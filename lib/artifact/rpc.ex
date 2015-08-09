defmodule RPC do
  @behaviour TCP

  def start_link do
    TCP.Supervisor.start_link(
      {:local, __MODULE__},
      __MODULE__,
      [],
      record(:tcp_option,
             [:binary, {:packet, 4}, {:active, true}, {:reuseaddr, true}],
             port: Config.get(:rpc_port),
             max_processes: Config.get(:rpc_max_processes))
    )
  end

  def stop, do: TCP.stop(__MODULE__)
  def init(_args), do: {:ok, {}}

  def handle_call(socket, data, state), do: dispatch(Config.node_info, :erlang.binary_to_term(data), state)

  @type state :: any
  @typep socket :: state
  @spec dispatch(state,
                 :node_info |
                 :node_list |
                 { :list, node } |
                 { :get, record(:data) } |
                 { :delete, record(data) } |
                 { :check_node, node } |
                 { :route, any } |
                 { :put, any } |
                 term,
                 state) :: reply
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
  defp dispatch(_socket, {:put, data}, state) when is_record(data, :data) do
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

  defp request(node, message) do
    case do_request(node, message) do
      {:ok, result}    -> result
      {:error, reason} ->
        Logging.warn("request(#{node}, #{message}): #{inspect reason}")
        {:error, reason}
    end
  end


  defp local?(node), do: node == Config.get(:node)

  def node_info(node) do
    case local?(node) do
      true -> Config.node_info()
      _    -> request(node, :node_info)
    end
  end


  def node_list(node) do
    case local?(node) do
      true -> Hash.node_list()
      _    -> request(node, :node_list)
    end
  end


  def list(node, bucket) do
    case local?(node) do
      true -> Store.list(bucket)
      _    -> request(node, {:list, bucket})
    end
  end


  def get(node, data) do
    case local?(node) do
      true -> Store.get(data)
      _    -> request(node, {:get, data})
    end
  end


  def put(node, data) do
    case local?(node) do
      true -> Store.put(data)
      _    -> request(node, {:put, data})
    end
  end


  def delete(node, data) do
    case local?(node) do
      true -> Store.delete(data)
      _    -> request(node, {:delete, data})
    end
  end


  def check_node(node, node2) do
    case local?(node) do
      true -> Membership.check_node(node2)
      _    -> request(node, {:check_node, node2})
    end
  end


  def route(node, request) do
    case local?(node) do
      true ->  {:error, :ewouldblock}
      _    ->  request(node, {:route, request})
    end
  end

end
