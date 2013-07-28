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
  
  use Application.Behaviour


  def config(keys, acc) when length(keys) == 0 do
    acc 
  end

  def config([key | tail], acc) do
    case :application.get_env(:artifact, key) do 
      {:ok, value } -> config(tail, [ {key, value} | acc ])
      :undefined    -> config(tail, acc)
    end
  end

  def start(_type, _args) do 
    args = config([
            :rpc, :interfaces, :buckets, :vnodes, :tables, :store,
            :participants, :read_participants, :write_participants,
            :hostname, :logging
      ], [])
    :artifact_supervisor.start_link(args)
  end

  def start do
    :application.start(:artifact) 
  end

end
