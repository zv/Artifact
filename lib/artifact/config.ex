defmodule Artifact.Config do
  @moduledoc """
  Loads, processes and sanitizes application environment data.
  """
  use GenServer

  def start_link(args) do
    GenServer.start_link({:local, __MODULE__}, __MODULE__, args, _options = [])
  end

  def init(args) do
    :ets.new(:config, [:set, :private, :named_table])

    # Load our keys
    Enum.each(args, fn({key, value}) ->
      :ets.insert(:config, {key, value})
    end)

    {:ok, hostname} = case Keyword.get(args, :hostname) do
      nil      -> :inet.gethostname
      hostname -> {:ok, hostname}
    end

    {:ok, address} = :inet.getaddr(hostname, :inet)
    port = Keyword.get(args, :rpc) |> Keyword.get(:port)
    :ets.insert :config, {:node, {address, port}}

    # Total buckets given by 2 ^ (log(buckets) / log(2))
    :ets.insert :config, {:buckets, Keyword.get(args, :buckets)}

    {:ok, []}
  end

  def terminate(_reason, _state), do: :ets.delete(:config)

  @doc """
  Specifies the daemon timeout window used by RPC
  """
  @daemon_timeout 3000
  @daemon_timer 1000
  def timer, do: @daemon_timer
  def timeout, do: @daemon_timeout


  @doc """
  Derive the current values of configuration parameters
  """
  @spec get(nonempty_list(binary()), tuple()) :: tuple()
  def get(keys, state) when is_list(keys) do
    {:reply, do_get(keys, []), state}
  end

  def get(key, state), do: {:reply, do_get(key), state}

  defp do_get(key) do
    case :ets.lookup(:config, key) do
      [{^key,value} | _ ] -> value
      _ -> nil
    end
  end

  defp do_get([], list_of_values) do
    Enum.reverse(list_of_values)
  end

  defp do_get([key|rest], list_of_values) do
    do_get(rest, [do_get(key)|list_of_values])
  end

  def node_info, do: GenServer.call(__MODULE__, :node_info)

  # Behaviour Callbacks

  def node_info(state) do
    [local_node, vnodes] = do_get([:node, :vnodes], [])
    info = [{:vnodes, vnodes}]
    {:reply, {:node_info, local_node, info}, state}
  end

  def stop, do: GenServer.call(__MODULE__, :stop)
  def get(key), do: GenServer.call(__MODULE__, {:get, key})

  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}
  def handle_call({:get, key}, _from, state), do: get(key, state)
  def handle_call(:node_info, _from, state), do: node_info(state)
  def handle_cast(_msg, state), do: super(_msg, state)
  def handle_info(_msg, state), do: super(_msg, state)
  def code_change(_old, state, _extra), do: super(_old, state, _extra)
end
