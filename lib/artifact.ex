defmodule Artifact do
  @moduledoc """

  Artifact is a performant, scalable distributed key-value store system,
  whose intellectual ancestor is Amazon's Dynamo database model, but serves as
  one of (if not the only) implementation of the SILT database concept.

  It's open source and freely available on github. Unstructured and permissive
  in what it accepts, strict in what it delivers -- perfect for storing large
  numbers of small (<128k) data records which need to be read or processed in a
  realtime, low-latency environment.
     
  This is the main module in the Artifact repository. 

  """ 
  
  def config([], acc) do
    acc 
  end

  @doc """
    Returns Artifact's current configuration 
  """
  def config([key | rest], acc) do
    case :application.get_env(:artifact, key) do 
      {:ok, value } -> config(rest, [ {key, value} | acc ])
      :undefined     -> config(rest, acc)
    end
  end

  def start(_type, _args) do 
    args = config([
            :n, :r, :w,
            :store,
            :buckets, :tables, :vnodes,
            :rpc_processes_ceiling, :rpc_port,
            :max_connections, :interfaces
      ], [])
    :artifact_supervisor.start_link(args)
  end

  def start do
    :application.start(:artifact) 
  end

end
