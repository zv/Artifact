defmodule Artifact.Config do
  use GenServer.Behaviour

  def start_link(args) do
    :gen_server.start_link(Artifact.Config, __MODULE__, args)
  end

  def init(args) do 
    # Elixir has not ported a library wrapping OTP ETS, so we use the native
    # interface here
    :ets.new :config, [:set, :private, :named_table]

    # Load our keys
    Enum.each(args, fn({k, v}) ->
      :ets.insert :config, {k, v}  
      end
    )

    {:ok, hostname} = case Keyword.get(args, :hostname) do 
      nil  -> :inet.gethostname()
      hostname -> {:ok, hostname}
    end
    {:ok, address} = :inet.getaddr(hostname, :inet) 
    port = Keyword.get(:rpc_port, args)
    :ets.insert :config, {:node, {address, port}} 

    # Total buckets given by 2 ^ (log(buckets) / log(2))
    :ets.insert :config, {:buckets, Keyword.get(:buckets, args)} 

    {:ok, []}
  end
end
