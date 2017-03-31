defmodule Artifact.TCP.Monitor do
  use GenServer


  def start_link(name) do
    GenServer.start_link(__MODULE__, [], name: name)
  end

  def stop(server_ref) do
    GenServer.call(server_ref, :stop)
  end

  def register(server_ref, pid), do: GenServer.call(server_ref, {:register, pid})
  def increment(server_ref, pid), do: GenServer.cast(server_ref, {:increment, pid})
  def decrement(server_ref, pid), do: GenServer.cast(server_ref, {:decrement, pid})
  def info(server_ref, key), do: GenServer.call(server_ref, {:info, key})

  def init(_args), do: {:ok, {_refs = [], _pids = []}}

  def handle_call({:register, pid}, _from, {monitor_refs, pids}) do
    {:reply, :ok, {[Process.monitor(pid) | monitor_refs], pids}}
  end
  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}
  def handle_call({:info, key}, _from, state), do: {:reply, state_to_info(state, key), state}
  def handle_call(_message, _from, state), do: {:reply, :ok, state}

  def handle_cast({:increment, pid}, {monitor_refs, pids}) do
    {:noreply, {monitor_refs, [pid | pids]}}
  end

  def handle_cast({:decrement, pid}, {monitor_refs, pids}) do
    {:noreply, {monitor_refs, :lists.delete(pid, pids)}}
  end

  def handle_cast(_message, state), do: {:noreply, state}

  def handle_info({:DOWN, monref, _type, pid, _info}, {monitor_refs, pids}) do
    Process.demonitor(monref)
    {:noreply, {:lists.delete(monref, monitor_refs), :lists.delete(pid, pids)}}
  end

  def handle_info(_info, state), do: {:noreply, state}

  def terminate(_reason, _state), do: :ok
  defp state_to_info({_monitor_refs, pids}, :curr_connections), do: length(pids)
  defp state_to_info(_state, _key), do: :undefined

  def code_change(_old_version, state, _extra), do: {:ok, state}
end
