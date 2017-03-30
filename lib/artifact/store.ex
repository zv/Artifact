defmodule Artifact.Store do
  @moduledoc """
    Store is simply an abstract module who derives the implementation of the
    storage at runtime.
    """
  alias Artifact.Config

  def start_link do
    storage = Config.get(:store) # This should return the proper module that handles storage
    apply(storage, :start_link, [__MODULE__])
  end

  def stop, do: GenServer.call(__MODULE__, stop)

  ## Client API
  def list(bucket), do: GenServer.call(__MODULE__, {:list, bucket})
  def get(data), do: GenServer.call(__MODULE__, {:get, data})
  def match(data), do: GenServer.call(__MODULE__, {:match, data})
  def put(data), do: GenServer.call(__MODULE__, {:put, data})
  def delete(data), do: GenServer.call(__MODULE__, {:delete, data})
  def info(name), do: GenServer.call(__MODULE__, {:info, name})
  def bucket(name), do: GenServer.call(__MODULE__, {:bucket, name})
end
