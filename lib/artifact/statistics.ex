defmodule Artifact.Statistics do
  @moduledoc """
  Artifact.Statistics stores and retrieves system operational details such as
  the uptime, number of active processes, artifact & erlang version, as well as
  statistics about the number of read, written and stored records.

  ## Statistics
  Each statistic can be individually queried through the `stat/2` interface or
  can be returned in aggregate through `all/0`

 | Statistic               | Notes                                                                              |
 | ------------------------|------------------------------------------------------------------------------------|
 | :uptime                 | Current node uptime, returned an an {megasec, second, millisec} Erlang-style tuple |
 | :time                   | Current node time, returned as a Erlang-style time-tuple (see `:uptime`)           |
 | :version                | Application configuration `:version` string                                        |
 | :bytes                  | How many bytes have been stored                                                    |
 | :current_items          | How many items have been stored                                                    |
 | :artifact_node          | inet-style local node IP/Port tuple                                                |
 | :artifact_quorum        | 3-tuple of Nodes N, R, W                                                           |
 | :artifact_buckets       | # of node buckets                                                                  |
 | :artifact_vnodes        | # of vnodes                                                                        |
 | :artifact_store         | Backing store module                                                               |
 | :current_connections    | Current # connections to memcache                                                  |
 | :artifact_current_nodes | Current connections IP/port tuples                                                 |
 | :vm_processes           | :erlang.system_info(:process_count)                                                |
 | :vm_version             | :erlang.system_info(:version)                                                      |
  """
  require Artifact
  alias Artifact.{Config, Store, Memcache, Hash}

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_arguments) do
    [node, quorum, buckets, vnodes, store] =
      Config.get([:node, :quorum, :buckets, :vnodes, :store])
    # TODO: Could be refactored into an Agent
    {:ok, %{
        boot_time: :erlang.timestamp(),
        # cmd_get: 0,
        # cmd_set: 0,
        # bytes_read: 0,
        # bytes_written: 0,
        node: local,
        quorum: quorum,
        buckets: buckets,
        vnodes: vnodes,
        store: store
     }}
  end

  def stop, do: GenServer.call(__MODULE__, :stop)
  def terminate(_reason, _state), do: :ok

  @doc ~S"""
  Joins a charlist with a delimiter

  ## Examples

      iex> join "abc", "."
      "a.b.c"

      iex> join "", "."
      ""

  """
  defp join(lst, delimiter) do
    String.split(lst, "", trim: true) |> Enum.join(delimiter)
  end

  defp increment(key, state), do: increment(key, 1, state)
  # :bytes_read gives a list of Records, which must be preprocessed.
  defp increment(:bytes_read, addend, state) when is_list(addend) do
    total = Enum.reduce(addend, 0, &(&1 + Artifact.data(datum, :value)))
    increment(:bytes_read, total, state)
  end
  defp increment(key, addend, state) do
    {:noreply,
     Keyword.update(state, key, addend, &(&1 + addend))}
  end

  # Interfaces
  def handle_call({:increment, key}, _from, state), do: increment(key, state)
  def handle_call({:increment, key, addend}, _from, state), do: increment(key, addend, state)

  defp stat(name, state) do
    case name do
      :uptime        -> {:uptime, "#{:timer.now_diff(:erlang.timestamp(), state.boot_time)}"}
      :time          -> {:time, "#{:erlang.timestamp()}"}
      :version       -> {:version, Application.fetch_env(:artifact, :version)}
      :bytes         -> {:bytes, "#{Store.info(:bytes)}"}
      :current_items -> {:current_items, "#{Store.info(:size)}"}
      :artifact_node    -> {:artifact_node, "#{state[:node]}"} # TODO: write proper conversion
      :artifact_quorum  -> {:artifact_quorum, "#{state[:quorum]}"}
      :artifact_buckets -> {:artifact_buckets, "#{state[:buckets]}"}
      :artifact_vnodes  -> {:artifact_vnodes, "#{state[:vnodes]}"}
      :artifact_store   -> {:artifact_store, "#{state[:store]}"}
      :current_connections    -> {:current_connections, "#{Memcache.current_connections()}"}
      :artifact_current_nodes ->
        {:node_list, nodes} = Hash.node_list()
        node_lst = Enum.map(nodes, node_to_list) |> Enum.sort |> join
        {:artifact_current_nodes, node_lst}
      :vm_processes -> {:vm_processes, "#{:erlang.system_info(:process_count)}"}
      :vm_version   -> {:vm_version, "#{:erlang.system_info(:version)}"}
      nm when nm in [:cmd_getnm, :cmd_setnm, :bytes_readnm, :bytes_written] ->
         {nm, "#{state[nm]}"}
      other ->
        # Logging
        IO.puts("#{other}")
        {:error, :not_found}
    end
  end
  def stat, do: GenServer.call(__MODULE__, :stat) # Interface

  defp all(state) do
    Enum.map([:uptime,:time,:version,:bytes, :current_items,:current_connections,
              :cmd_get,:cmd_set, :bytes_read,:bytes_write, :artifact_node,
              :artifact_quorum,:artifact_buckets, :artifact_vnodes, :artifact_store,
              :artifact_current_nodes, :artifact_unreconciled_get, :vm_procs,:vm_version],
      &stat(&1, state))
  end
  def all, do: GenServer.call(__MODULE__, :all) # Interface

  # OTP Callbakcs
  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}
  def handle_info(_info, state), do: {:noreply, state}
  def code_change(_old_version, state, extra), do: {:ok, state}
end
