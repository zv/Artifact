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


  defrecord Data, 
    key: nil, bucket: nil, flags: nil, vector_clocks: nil, checksum: nil, 
    value: nil, last_modified: { { 1970, 1, 1 }, { 0, 0, 0 } } do
      @moduledoc """
      The data record is responsible for storing all the information describing
      a value (and it's associated key), it contains:

      * `:key` - The key used to find the data;
      * `:bucket` - The bucket (or buckets) where this data presently resides as
          the last synchronization with another host with new information.
      * `:flags` - An integer containing the flags of this account.
         filesystem path. This information may be used later by the finder
         to retrieve the template source
      * `:vector_clocks` - The vector clock chain for to reconcile future changes.
      * `:checksum` - The checksum of the data.
      * `:last_modified` - Last modification time.
      """
    end

  defrecord TCPOptions, 
    listen: [{:active, false}, :binary, {:packet, :line}, {:reuseaddr, true}],
    port: 11211, max_processes: 8, max_restarts: 3, time: 60, shutdown: 2000,
    accept_timeout: :infinity, accept_error_sleep_time: 3000, recv_length: 0,
    recv_timeout: :infinity 


  defmacro server do
    __MODULE__
  end

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
