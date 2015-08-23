defmodule Artifact.Sync do
  @moduledoc """
  Sync is responsible for comparing the contents of individual buckets,
  reconciling changes, removing stale entries and synchronizing them if needed.
  """

  alias Artifact.Logging
  alias Artifact.Store
  alias Artifact.RPC

  def start_link() do
    :gen_fsm.start_link({:local, __MODULE__}, __MODULE__, [], _opts = [])
  end

  def terminate(_reason, _statename, _state_data), do: :ok

  def retrieve_data(_node, []), do: :ok

  def retrieve_data(node, [metadata|t]) do
    case Store.get(metadata) do
      # Check if we've recieved a data record
      {:data, data} -> retrieve_data(node, t)
      :undefined ->
        case(RPC.get(node, metadata)) do
          {:data, data} ->
            Store.put(data)
            retrieve_data(node, t)
          {:error, reason} ->
            Logging.warn("retrieve_data/2: #{inspect error: reason}")
            {:error, reason}
          :undefined -> retrieve_data(node, t)
        end
    end
  end


  defp do_update_bucket(_bucket, []), do: {:error, :enodata}

  defp do_update_bucket(bucket, [node | rest]) do
    case Artifact.RPC.list(node, bucket) do
      {:list_of_data, data_list} -> retrieve_data(node, data_list)
      {:error, reason} ->
        Logging.warn("do_update_bucket/2: #{inspect error: reason}")
        do_update_bucket(bucket, rest)
    end
  end

  defp do_update_bucket(bucket) do
    {:nodes, nodes} = Hash.find_nodes(bucket)
    do_update_bucket(bucket, nodes -- [Config.get(:node)])
  end

  defp do_delete_bucket([metadata | t]) do
    Store.delete(metadata)
    do_delete_bucket(t)
  end

  defp do_delete_bucket(bucket) do
    {:list_of_data, listOfData} = Store.list(bucket)
    do_delete_bucket(listOfData)
  end

  def ready({:update_bucket, bucket}, state) do
    do_update_bucket(bucket)
    {:next_state, :ready, state, Config.timer}
  end

  def ready({:delete_bucket, bucket}, state) do
    do_delete_bucket(bucket)
    {:next_state, :ready, state, Config.timer}
  end

  def ready(:timeout, state) do
    case(Artifact.Hash.choose_bucket_randomly()) do
      {:bucket, bucket} -> do_update_bucket(bucket)
      _                 -> :nop
    end
    {:next_state, :ready, state, Config.timer}
  end

  def handle_event(:stop, _state_name, state_data), do: {:stop, :normal, stateData}
  def handle_sync_event(_event, _from, _state_name, state_data), do: {:next_state, :wait, stateData, @timeout}
  def handle_info(_info, _state_name, state_data), do: {:next_state, :ready, stateData, Config.timer}
  def code_change(_oldVsn, _state_name, state_data, _extra), do: {:ok, :ready, stateData}
  def stop(), do: :gen_fsm.send_all_state_event(__MODULE__, :stop)
  def update_bucket(bucket), do: :gen_fsm.send_event(__MODULE__, {:update_bucket, bucket})
  def delete_bucket(bucket), do: :gen_fsm.send_event(__MODULE__, {:delete_bucket, bucket})
end
