defmodule Artifact.Memcache do

  # ERROR - must add 'current_connections'

  import Artifact, only: [server_options: 1]
  alias Artifact.Config

  def start_link() do
    [port: port, process_limit: limit] = Config.get(:interfaces)[:memcache]
    options = Artifact.server_options([port: port, max_processes: limit])
    opts = [port: port]
    {:ok, _} = :ranch.start_listener(
      :artifact,                  # Application
      limit,                      # Number of spawned acceptors
      :ranch_tcp,                 # Type
      opts,                       # Socket options
      Artifact.Memcache.Acceptor, # Pool module
      [])                         # Arguments
  end

  def stop do
    :ranch.stop_listener(Artifact.Memcache.Acceptor)
  end




end

defmodule Artifact.Memcache.Acceptor do
  require Artifact
  alias Artifact.{Coordinator, Statistics, Version}
  @timeout 2

  def listen(state) do
    case recv(state) do
      {:reply, to_send, state} ->
        state.transport.send(to_send)
        listen(state)
      {:noreply, state} -> listen(state)
      {:close, state} -> :tcp_closed
      {:close, to_send, state} -> state.transport.send(to_send)
    end
  end

  def recv(state) do
    case state.transport.recv(state.socket,
          state.opts.recv_length,
          state.opts.recv_timeout) do
      {:ok, recieved} ->
        dispatch(recieved, state)
      {:error, reason} ->
        # Logging
        IO.puts("memcache recv error: #{reason}")
        :ok = state.transport.close(state.socket)
        {:close, state}
    end
  end

  defp reply_error(msg, state) do
    # TODO: Logging
    IO.puts("reply_error #{msg}")
    {:close, "SERVER_ERROR #{msg}\r\n", state}
  end

  # 'GET' command
  defp dispatch(["get", key], state), do: do_get(key, state, false)
  defp dispatch(["gets", key], state), do: do_get(key, state, true)
  defp do_get(key, state, checksum?) do
    case Coordinator.route({:get, Artifact.data(key: key)}) do
      datum when is_list(datum) ->
        {:ok, unique_bin} = Version.merge_clocks(datum)
        response = get_response(datum, checksum?, unique_bin)
        Statistics.increment(:cmd_get)
        Statistics.increment(:bytes_read, datum)
        {:reply, [response | "END\r\n"], state}
      :undefined -> {:reply, <<"END\r\n">>, state}
      _else -> reply_error("Failed to read", state)
    end
  end
  defp merge_clocks(checksum), do: <<hash::size(64) - integer>> = checksum
  # Actually build the GET response
  defp get_response(datum, checksum?, unique_bin) do
    Enum.map(datum, fn(elt) ->
      key   = Artifact.data(datum, :key)
      flags = Artifact.data(datum, :flags)
      value = Artifact.data(datum, :value)
      [
        "VALUE #{key} #{flags} #{:erlang.byte_size(value)}",
        (if checksum?, do: " #{merge_clocks(unique_bin)}", else: []),
        "\r\n",
        value,
        "\r\n"
      ]
    end)
  end

  # 'SET' command
  defp dispatch(["set", _key, _flags, "0", _bytes] = datum, state) do
    state.transport.setopts([packet: :raw])
    result = do_set(datum, state)
    state.transport.setopts([packet: :line])
  end
  defp dispatch(["set", _key, _flags, _expiration, _bytes], state) do
    {:reply, <<"CLIENT_ERROR Exptime must be zero.\r\n">>, state}
  end
  defp do_set(["set", key, flags, "0", bytes], state) do
    case state.transport.recv(state.socket, List.to_integer(bytes), @timeout) do
      {:ok, value} ->
        state.transport.recv(state.socket, 2, @timeout)
        case Coordinator.route(:put, Artifact.data(key: key, flags: flags, value: value)) do
          :ok ->
            state.transport.send(<<"STORED\r\n">>)
            Statistics.increment(:cmd_set)
            Statistics.increment(:bytes_written, value)
            {:noreply, state}
          _else -> reply_error("Write failure", state)
        end
      _else -> {:noreply, state}
    end
  end

  # 'VERSION' command
  defp dispatch(["version"], state) do
    version = Application.fetch_env(:artifact)[:version]
    {:reply, "VERSION #{version}\r\n", state}
  end


end
