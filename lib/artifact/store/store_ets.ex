defmodule Artifact.Store.ETS do
  require Artifact
  require Record

  def start_link(server), do: GenServer.start_link(__MODULE__, :ok, name: __MODULE__)

  def init(_args) do
    :ets.new(__MODULE__, [:set, :private, :named_table, {:keypos, 2}])
    {:ok, []}
  end

  def terminate(_reason, _state) do
    :ets.delete(__MODULE__)
    :ok
  end

  @doc """
  List the documents stored in a particular bucket
  """
  defp do_list(bucket, state) do
    head = Aritfact.data(
      key: :'$1',
      bucket: bucket,
      last_modified: :'$2',
      vector_clocks: :'$3',
      checksum: :'$4',
      flags: :'_',
      value: :'_'
    )

    condition = []

    body = Artifact.data(
      key: :'$1',
      bucket: bucket,
      last_modified: :'$2',
      vector_clocks: :'$3',
      checksum: :'$4'
    )

    {:reply,
     {:list_of_data,
      :ets.select(__MODULE__, [{head, condition, body}])},
     state}
  end

  @doc """
  Fetch a document by key
  """
  defp do_get(datum, state) do
    key = Artifact.data(datum, :key)
    case :ets.lookup(__MODULE__, key) do
      [^datum] -> {:reply, datum, state}
      _ -> {:reply, :undefined, state}
    end
  end

  @doc """
  Match a document by key
  """
  defp do_match(datum, state) do
    key = Artifact.data(datum, :key)
    case :ets.match(__MODULE__, key) do
      [^datum] -> {:reply, datum, state}
      _ -> {:reply, :undefined, state}
    end
  end

  @doc """
  Insert a document
  """
  defp do_put(datum, state) when Record.is_record(datum, :data) do
    case :ets.lookup(__MODULE__, Artifact.data(datum, :key)) do
      [stored] ->
        if :vclock.descends(Artifact.data(datum, :vector_clocks), Artifact.data(stored, :vector_clocks)) do
          insert_datum(datum, state)
        else
            {:reply,
             {:error, "stale"}, state}
        end
      _ -> insert_datum(datum, state)
    end
  end
  defp do_put(_datum, _state), do: :ok

  @doc """
  Handles the actual machinery of inserting a document into ETS
  """
  defp insert_datum(datum, state) do
    :ets.insert(__MODULE__, datum)
    {:reply, :ok, state}
  end

  @doc """
  Delete a document
  """
  defp do_delete(datum, state) do
    key = Artifact.data(datum, :key)
    case :ets.lookup(__MODULE__, key) do
      [^datum] ->
        :ets.delete(__MODULE__, key)
        {:reply, :ok, state}
      _ -> {:reply, :undefined, state}
    end
  end

  defp info(:bytes, state) do
    free = ((:erlang.system_info(:wordsize) * :ets.info(__MODULE__, :memory)) +
            :erlang.memory(:binary))
    {:reply, free, state}
  end
  defp info(:size, state), do: {:reply, :ets.info(__MODULE__, :size), state}
  defp info(name, state), do: {:reply, :undefined, :state}

  # Callbacks
  def handle_call(:stop, _from, state), do: {:stop, :normal, :stopped, state}
  def handle_call({:list, bucket}, _from, state), do: do_list(bucket, state)
  def handle_call({:get, datum}, _from, state), do: do_get(datum, state)
  def handle_call({:put, datum}, _from, state), do: do_put(datum, state)
  def handle_call({:delete, datum}, _from, state), do: do_delete(datum, state)
  def handle_call({:info, name}, _from, state), do: info(name, state)
  def handle_cast(_msg, state), do: {:noreply, state}
  def handle_info(_msg, state), do: {:noreply, state}
  def code_change(_old_version, state, _extra), do: {:ok, state}
end
