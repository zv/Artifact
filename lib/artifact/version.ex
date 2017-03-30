defmodule Artifact.Version do
  @cas_bits 64
  alias Artifact.Config

  def start_link() do
    GenServer.start_link(__MODULE__, __MODULE__, [], name: __MODULE__)
  end

  def init(_args), do: {:ok, []}
  def terminate(_reason, _state), do: :ok

  def update(datum, state) do
    next_vclock = :vclock.increment(Config.get(:node), data(datum, :vector_clocks))
    {:reply,
     {:ok,
      data(last_modified: :erlang.now(), vector_clocks: next_vclock), state}}
  end

  # Take a list of events and order them into a unique list by removing any that
  # descend from a prior datum
  defp do_order([], []), do: :undefined
  defp do_order([], lst), do: lst
  defp do_order([datum|rest], lst) do
    vclock = data(datum, :vector_clocks)
    comparator = fn(n_vclock) ->
      :vclock.descends(data(other, :vector_clocks), vclock)
    end

    if Enum.any?(rest, comparator) do
      do_order(rest, lst)
    else
      do_order(rest, [datum | lst])
    end
  end

  def order(lst, state), do: {:reply, do_order(lst, []), state}
  def order(_lst, state), do: {:reply, :undefined, state}

  @doc """
  Memcaches protocols limit us in how we can reply with message consistency
  data. Instead of breaking protocol compatibility, Artifact packs a list of
  elements checksums into a single value.
  """
  def merge_clocks(lst) when length(lst) > 15 do
    {:error, IO.puts("merge_clocks/1 (lst is too long) #{inspect length(lst)}") }
  end
  def merge_clocks(lst) do
    # The actual packing of data is as follows:
    #   First 4 bits: #(data)
    #   [Each data's checksum]:(60/#(data))
    #   And some padding
    length = length(lst)
    bits = trunc(60/length)
    rest_bits = 128 - bits
    lst = Enum.map lst, fn(dt) ->
      <<checksum:bits, _:rest>> = data(dt, :checksum)
      checksum
    end
    merge_clocks(lst, bits, length, 4)
  end
  def merge_clocks([], _bits, result, result_bits) do
    padding = @cas_bits - result_bits
    { :ok, <<result:result_bits, 0:padding>> }
  end
  def merge_clocks([checksum | rest], bits, result, result_bits) do
    result_bits = result_bits + each_bits
    # pack our checksums
    <<result:result_bits>> = <<result:result_bits, checksum:bits>>
    merge_clocks(rest, bits, result, result_bits)
  end

  ## Callbacks

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :stopped, state}
  end
  def handle_call({:update, data}, _from, state), do: update(data, state)
  def handle_call({:order, lst}, _from, state), do: order(lst, state)
  def handle_cast(_msg, state), do: {:noreply, state}
  def handle_info(_info, state), do: {:noreply, state}
  def code_change(_old_version, state, _extra), do: {:ok, state}

  def stop, do: GenServer.call(__MODULE__, :stop)
  def update(data), do: GenServer.call(__MODULE__, {:update, data})
  def order(lst), do: GenServer.call(__MODULE__, {:stop, lst})
end
