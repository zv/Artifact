defmodule Artifact.Config do
  @moduledoc """
  Loads, processes and sanitizes application environment data.
  """
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, {:ok, args}, name: __MODULE__)
  end

  def init({:ok, environment}) do
    {:ok, agent} = Agent.start_link fn -> %{} end

    # Load our keys into the agent
    Enum.each(environment, fn({key, value}) ->
      Agent.update(agent, fn map -> Map.put(map, key, process(key, environment)) end)
    end)

    # TODO: hack to get the node info in there
    Agent.update(agent, fn map -> Map.put(map, :node, process(:node, environment)) end)

    {:ok, %{agent: agent}}
  end

  @doc """
  Process verifies, corrects or modifies Mix environment keys that are loaded
  into Config on `init` or returns the configuration value if no special
  processing is required.
  """
  def process(key, envs), do: envs[key]

  def process(:quorum, envs) do
    {n, r, w} = quorum = envs[:quorum]

    # Check our quorum parameters
    if (r + w > n) do :ok
    else exit("The quorum is incorrectly configured, verify that R + W > N")
    end

    if (w > n / 2) do :ok
    else exit("The quorum is incorrectly configured, verify that W > N / 2")
    end

    quorum
  end


  def process(:buckets, envs) do
    :math.pow(2, trunc(:math.log2(envs[:buckets])))
  end

  def process(:node, envs) do
    # if the hostname isn't specified, rely on the network to fetch it
    {:ok, hostname} = case Keyword.get(envs, :hostname) do
                        nil      -> :inet.gethostname
                        hostname -> {:ok, hostname}
                      end
    {:ok, address} = :inet.getaddr(hostname, :inet)
    port = Keyword.get(envs, :rpc) |> Keyword.get(:port)

    {address, port}
  end


  @doc """
  Fetches a configuration parameter from the agent process or ETS.

  ## Examples

  iex> Artifact.Config.get :vnodes
  2048
  """
  @spec get(nonempty_list(binary()), tuple()) :: tuple()
  def get(keys, state = %{agent: agent}) when is_list(keys) do
    {:reply, do_get(keys, agent), state}
  end

  def get(key, state = %{agent: agent}), do: {:reply, do_get(key, agent), state}

  defp do_get(keys, agent) when is_list(keys) do
    Enum.map(keys, fn key -> do_get(key, agent) end)
  end
  defp do_get(key, agent), do: Agent.get(agent, fn map -> map[key] end)

  @doc """
  Reply with an aggregate of local node configured and derived system information.
  Because some information cannot be predicted from the configuration _alone_
  (hostname, etc), you can also use this to verify appropriate settings.

  ## Examples
  iex> Artifact.config.node_info
  {:node_info, {{127,0,0,1}, 10070}, [{:vnodes, 256}]}
  """
  def node_info(state = %{agent: agent}) do
    [local_node, vnodes] = do_get([:node, :vnodes], agent)
    info = [{:vnodes, vnodes}]
    {:reply, {:node_info, local_node, info}, state}
  end
  def node_info, do: GenServer.call(__MODULE__, :node_info)

  def get(key), do: GenServer.call(__MODULE__, {:get, key})

  # OTP Callbacks
  def terminate(reason, %{agent: agent}), do: Agent.stop(agent)
  def stop, do: GenServer.call(__MODULE__, :stop)
  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}
  def handle_call({:get, key}, _from, state), do: get(key, state)
  def handle_call(:node_info, _from, state), do: node_info(state)
  def handle_cast(msg, state), do: super(msg, state)
  def handle_info(msg, state), do: super(msg, state)
  def code_change(old, state, extra), do: super(old, state, extra)
end
