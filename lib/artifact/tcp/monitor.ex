defmodule Artifact.TCP.Monitor do
  def start_link(name), do: :gen_server.start_link(name, __MODULE__, [], [])

  def stop(serverRef), do: :gen_server.call(serverRef, :stop)

  def register(serverRef, pid), do: :gen_server.call(serverRef, {:register, pid})

  def increment(serverRef, pid), do: :gen_server.cast(serverRef, {:increment, pid})

  def decrement(serverRef, pid), do: :gen_server.cast(serverRef, {:decrement, pid})

  def info(serverRef, key), do: :gen_server.call(serverRef, {:info, key})

  def init(_args), do: {:ok, {_monitorRefs = [], _pids = []}}

  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}

  def handle_call({:register, pid}, _from, {monitorRefs, pids}) do
    {:reply, :ok, {[:erlang.monitor(:process, pid) | monitorRefs], pids}}
  end

  def handle_call({:info, key}, _from, state), do: {:reply, state_to_info(state, key), state}

  def handle_call(_message, _from, state), do: {:reply, :ok, state}

  def handle_cast({:increment, pid}, {monitorRefs, pids}) do
    {:noreply, {monitorRefs, [pid | pids]}}
  end

  def handle_cast({:decrement, pid}, {monitorRefs, pids}) do
    {:noreply, {monitorRefs, :lists.delete(pid, pids)}}
  end

  def handle_cast(_message, state), do: {:noreply, state}

  def handle_info({:DOWN, monitorRef, _type, pid, _info}, {monitorRefs, pids}) do
    :erlang.demonitor(monitorRef)
    {:noreply, {:lists.delete(monitorRef, monitorRefs), :lists.delete(pid, pids)}}
  end

  def handle_info(_info, state), do: {:noreply, state}

  def terminate(_reason, _state), do: :ok

  def code_change(_oldVsn, state, _extra), do: {:ok, state}

  defp state_to_info({_monitorRefs, pids}, :curr_connections), do: length(pids)

  defp state_to_info(_state, _key), do: :undefined
end
