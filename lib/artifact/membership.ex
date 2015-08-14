defmodule Artifact.Membership do
  @behaviour :gen_fsm

  def start_link do
    :gen_fsm.start_link({:local, __MODULE__}, __MODULE__, [], _opts = [])
  end

  def init(_args), do: {:ok, :ready, [], Config.timer}

  def terminate(_reason, _state_name, _state_data), do: :ok

  defp ping([], available, down), do: {available, down}
  defp ping([node|nodes], available, down) do
    case Artifact.RPC.node_info(node) do
      {:node_info, node, info} -> ping(nodes, [{node, info}|available], down)
      {:error, reason} ->
        Logging.warn("ping error: #{inspect reason}")
        ping(nodes, available, [node|down])
    end
  end


  defp node_manifest(node) do
    case Artifact.RPC.node_peers(node) do
      {:node_list, remote_nodes} ->
        {:node_list, local_nodes} = Artifact.Hash.node_list()
        nodes = (remote_nodes -- local_nodes) ++ (local_nodes -- remote_nodes)
        local = Artifact.Config.get(node)
        ping(nodes -- [local], [], [])
      {:error, reason} ->
        Logging.warn("node_manifest: error #{inspect reason}")
        {[], [node]}
    end
  end

  defp sync_buckets([], _local), do: :ok
  defp sync_buckets([{bucket, new_replica, old_replica}|replaced_buckets], local) do
    case {new_replica, old_replica} do
      {^new_replica, :undefined} -> Artifact.Sync.update_bucket(bucket)
      {:undefined, ^old_replica} -> Artifact.Sync.delete_bucket(bucket)
      _ -> :nop
    end
    sync_buckets(replaced_buckets, local)
  end
  defp sync_buckets(replaced), do: sync_buckets(replaced, Artifact.Config.get(:node))

  defp do_check_node({address, port}) do
    {available, down} = node_manifest({address, port})
    {:replaced_buckets, replaced} = Artifact.Hash.update_nodes(available, down)
    sync_buckets(replaced)
  end

  def ready({:check_node, var_node}, state) do
    do_check_node(var_node)
    {:next_state, :ready, state, Config.timer}
  end

  def ready(:timeout, state) do
    case(Artifact.Hash.choose_node_randomly()) do
      {:node, var_node} -> do_check_node(var_node)
      _ -> :nop
    end
    {:next_state, :ready, state, Config.timer}
  end

  def handle_event(:stop, _stateName, state), do: {:stop, :normal, state}

  def handle_sync_event(_event, _from, _stateName, state), do: {:next_state, :ready, state, @timeout}

  def handle_info(_info, _stateName, state), do: {:next_state, :ready, state, @timeout}

  def code_change(_oldVsn, _stateName, state, _extra), do: {:ok, :ready, state}

  def stop(), do: :gen_fsm.send_all_state_event(__MODULE__, :stop)

  def check_node(var_node), do: :gen_fsm.send_event(__MODULE__, {:check_node, var_node})

end
