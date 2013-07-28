defmodule Artifact.Logging do
  use GenServer.Behaviour

  def init(_args) do
    case Artifact.Config.get(:logging) do
      nil     -> { :ok, [] }
      logfile ->
        case File.open(logfile, [:write, :append]) do
          {:ok, io }        -> {:ok, [fd: io]}
          {:error, reason } -> {:error, reason}
        end
    end
  end

  def terminate(_reason, state) do
    case Keyword.get(state, :fd) do
      nil -> :ok
      fd  -> File.close(fd)
    end
  end

  def log(level, pid, file, line, data) do
    {{y, m, d}, {h, m, s}} = :erlang.localtime

    # flatten will recursively flatten so go wild! 
    flat_data = if is_list(data) do
      List.flatten(data)
    else
      data
    end
    
    # rather than using a lager custom formatter, we build our own here so that
    # we can flexibly support web interfaces that hook into log events.
    log_formatstring = "~2..0w-~2..0w-~2..0w-~2..0w-~4..0w [~s] (~p) ~s:~w: ~p\n"
    log_items        = [s, m, h, d, m, y, pid, file, line, flat_data] 
    case level do 
      :debug    ->
        :lager.debug(log_formatstring, log_items)
      :info     ->
        :lager.info(log_formatstring, log_items)
      :warning  ->
        :lager.warning(log_formatstring, log_items)
      :error    ->
        :lager.error(log_formatstring, log_items)
    end
  end

  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

  def handle_call(:stop, _from, state) do 
    # terminate any callee using a call/multicall on us
    {:stop, :normal, :stopped, state}
  end

  def handle_cast({:log, level, pid, file, line, data}, state) do 
    log(level, pid, file, line, data)
    {:noreply, state}
  end

  def stop do
    :gen_server.call(__MODULE__, stop)
  end

  def handle_info(info, state) do
    super(info, state)
  end

  def code_change(old_version, state, extra) do
    super(old_version, state, extra)
  end

end
