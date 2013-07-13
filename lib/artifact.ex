defmodule Artifact do
  use Application.Behaviour
  
  def config([], acc) do
    acc 
  end

  @doc """
    Returns Artifact's current configuration 
  """
  def config([key | rest], acc) do
    case System.get_env(:artifact, key) do 
      {:ok, value } -> config(rest, [ {key, value} | acc ])
      :undefined     -> config(rest, acc)
    end
  end

  # Application behaviour callbacks.
  def start(_type, _args) do
    Application.start(:artifact)
  end

  def stop(_state) do
    :ok
  end

end
