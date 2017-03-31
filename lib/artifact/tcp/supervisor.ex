defmodule Artifact.TCP.Supervisor do
  use Supervisor
  import Supervisor.Spec

  def start_link({:global, name}, module, args, options) do
    Supervisor.start_link(__MODULE__, [{:global, name}, module, args, options], [])
  end
  def start_link(name, module, args, options) do
    Supervisor.start_link(__MODULE__, [name, module, args, options], [name: name])
  end


  def stop(name) do
    case :erlang.whereis(name) do
      pid when is_pid(pid) ->
        :erlang.exit(pid, :normal)
        :ok
      _                    -> :not_started
    end
  end

  def init([name, mod, args, option]) do
    case(mod.init(args)) do
      {:ok, state}    -> listen(state, name, mod, option)
      {:stop, msg}    -> msg
      other           -> other
    end
  end

  def listen(state, name, module, option) do
    case :gen_tcp.listen(option[:port], option[:listen]) do
      {:ok, socket} ->
        monitor_name = monitor_name(name)
        children = [build_monitor_worker(monitor_name) |
                    build_acceptor_workers(name, socket, state, monitor_name, module, option)]
        supervise(children, strategy: :one_for_one)
      {:error, msg} ->
        IO.puts("listen(#{inspect module}) #{inspect msg}")
        {:stop, msg}
    end
  end

  defp monitor_name(prefix), do: :"#{prefix}.Monitor"
  defp acceptor_name(prefix, n), do: :"#{prefix}.Acceptor#{n}"

  defp build_monitor_worker({:global, name}), do: build_monitor_worker(name)
  defp build_monitor_worker(name) do
    # Build up a child specification given the
    worker(
      Artifact.TCP.Monitor,
      [name], # arguments
      [
        id: name,
        function: :start_link,
        restart: :permanent,
        shutdown: :brutal_kill
      ])
  end

  defp build_acceptor_workers({:global, name}, socket, state, monitor_name, module, option) do
    build_acceptor_workers(name, socket, state, {:global, monitor_name}, module, option)
  end
  defp build_acceptor_workers(name, socket, state, monitor_name, module, option) do
    max_proc = option[:max_processes]
    shutdown = option[:shutdown]
    Enum.map(1..max_proc, fn(n) ->
      acceptor = acceptor_name(name, n)
      worker(Artifact.TCP.Acceptor,
        # Arguments to the TCP acceptor
        [acceptor, socket, state, monitor_name, module, option],
        [
          id: acceptor,
          function: :start_link,
          restart: :permanent,
          shutdown: shutdown
        ]
      )
    end)
  end

end
