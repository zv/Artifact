defmodule Artifact.TCP.Supervisor do
  use Supervisor
  import Supervisor.Spec
  @max_processes 8
  @shutdown :shutdown

  def start_link(name, module, args, option) do
    Supervisor.start_link(name, __MODULE__,
                          [name, module, args, option])
  end

  def stop(name) do
    case Process.whereis(name) do
      pid when is_pid(pid) -> Process.exit(pid, :normal)
      _                    -> :not_started
    end
  end

  def init([name, mod, args, option]) do
    case(mod.init(args)) do
      {:ok, state}    -> listen(state, name, mod, option)
      {:stop, msg} -> msg
      other           -> other
    end
  end

  def listen(state, {dest, name}, module, option) do
    case :gen_tcp.listen(@port, @listen) do
      {:ok, socket} ->
        supervise(
          [
            build_monitor_worker({dest, name}),
            build_acceptor_workers(socket, state, {dest, name}, name, module, option)
          ],
          strategy: :one_for_one
        )
      {:error, msg} ->
        Logging.warn("listen(#{inspect module}) #{inspect msg}")
        {:stop, msg}
    end
  end

  defp monitor_name(prefix), do: "#{prefix}_monitor"
  defp acceptor_name(prefix, n), do: "#{prefix}_acceptor_#{n}"

  defp build_monitor_worker(identity = {_dest, name}) do
    # Build up a child specification given the
    worker(monitor_name(name), [identity], [
          function: :start_link,
          restart: :permanent,
          shutdown: :brutal_kill
        ])
  end

  defp build_acceptor_workers({dest, name}, socket, state,  monitor_base_name, module, option) do
    monitor_name = if dest == :local do
      monitor_base_name
    else
      {dest, monitor_base_name}
    end

    for n <- @max_processes do
      acceptor = acceptor_name(name, n)
      worker(acceptor,
             # Arguments to the TCP acceptor
             [{dest, acceptor}, socket, state, monitor_name, module, option],
             [
               function: :start_link,
               restart: :permanent,
               shutdown: @shutdown
             ]
      )
    end
  end
end
