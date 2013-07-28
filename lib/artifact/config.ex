defmodule Artifact.Config do
  use GenServer.Behaviour

  def start_link(args) do
    :gen_server.start_link(Artifact.Config, __MODULE__, args)
  end

  def init(args) do 
    # Elixir has not ported a library wrapping OTP ETS, so we use the native
    # interface here
    :ets.new :config, [:set, :private, :named_table]

    # Load our keys
    Enum.each(args, fn({k, v}) ->
      :ets.insert :config, {k, v}  
    end
    )

    {:ok, hostname} = case Keyword.get(args, :hostname) do 
      nil      -> :inet.gethostname
      hostname -> {:ok, hostname}
    end

    {:ok, address} = :inet.getaddr(hostname, :inet) 
    port = Keyword.get(args, :rpc) |> Keyword.get(:port)
    :ets.insert :config, {:node, {address, port}} 

    # Total buckets given by 2 ^ (log(buckets) / log(2))
    :ets.insert :config, {:buckets, Keyword.get(:buckets, args)} 

    {:ok, []}
  end

  def terminate(_reason, _state) do 
    :ets.delete(:config)
  end

  def do_get(key) do 
    case :ets.lookup(:config, key) do
      [{^key,value} | _ ] -> value
      _ -> nil
    end
  end

  def do_get([], list_of_values) do
    :lists.reverse(list_of_values)
  end

  def do_get([key|rest], list_of_values) do
    do_get(rest, [do_get(key)|list_of_values])
  end

  def stop do
    :gen_server.call(__MODULE__, :stop)
  end

  def get(key) do
    :gen_server.call(__MODULE__, {:get, key})
  end

  def node_info do
    :gen_server.call(__MODULE__, :node_info)
  end

  def get(list_of_keys, state) when is_list(list_of_keys) do
    {:reply, do_get(list_of_keys, []), state}
  end

  def get(key, state) do
    {:reply, do_get(key), state}
  end


  def node_info(state) do
    [local_node, vnodes] = do_get([:node, :vnodes], [])
    info = [{:vnodes, vnodes}]
    {:reply, {:node_info, local_node, info}, state}
  end

  # Behaviour Callbacks 

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :stopped, state}
  end

  def handle_call({:get, key}, _from, state) do
    Config.get(key, state)
  end

  def handle_call(:node_info, _from, state) do
    Config.node_info(state)
  end

  def handle_cast(_msg, state) do
    super(_msg, state)
  end

  def handle_info(_msg, state) do
    super(_msg, state)
  end

  def code_change(_old, state, _extra) do
    super(_old, state, _extra)
  end

end
