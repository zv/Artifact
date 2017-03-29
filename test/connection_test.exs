Code.require_file "test_helper.exs", __DIR__

defmodule ArtifactTest.Connection do
  use ExUnit.Case
  alias Artifact.Config
  alias Artifact.Connection
  import Artifact.TestMacros, only: :macros

  # setup_all do
  #   # IO.puts "Initializing Config Server..."
  #   # {:ok, config} = Config.start_link(
  #   #   [hostname: 'localhost',
  #   #    participants: 2,
  #   #    buckets: 2048,
  #   #    vnodes: 256,
  #   #    rpc: [port: 10070]]
  #   # )
  #   # {:ok, connection} = Connection.start_link()
  #   # spawn_link(echo_start)
  #   # {:ok, connection: connection }
  #   # { :ok, config: config }
  # end

  def test_api() do
    {:ok, listening_sock} = :gen_tcp.listen(11012, [:binary, {:packet, 4}, {:reuseaddr, :true}])
    test_api_accept(listening_sock)
  end

  def test_api_accept(sock) do
    {:ok, api_socket} = :gen_tcp.accept(sock)
    pid = spawn(__MODULE__, :test_api_proc, [api_socket])
    :gen_tcp.controlling_process(api_socket, pid)
    test_api_accept(sock)
  end

  def test_api_proc(api_socket) do
    receive do
      {:tcp, ^api_socket, _bin} -> :gen_tcp.send(api_socket, :erlang.term_to_binary(:ok))
    after 5000 -> false
    end
  end

  def test_api_send(pid) do
    {:ok, socket} = Connection.acquire(node2(), self())
    :ok = :gen_tcp.send(socket, :erlang.term_to_binary(:ok))
    send pid, (receive do
      {:tcp, ^socket, bin} -> :erlang.binary_to_term(bin)
    end)
  end

  test "acquire/2" do
    Config.start_link([
      rpc: [port: 11011],
      max_connections: 32,
      n: 3,
      number_of_buckets: 8,
      number_of_virtual_nodes: 2
    ])

    Connection.start_link()

    spawn_link(__MODULE__, :test_api, [])

    {:ok, socket} = Connection.acquire(node2(), self())
    {:ok, connections} = Connection.connections()
    assert length(connections) == 1

    {:ok, socket2}      = Connection.acquire(node2(), self())
    {:ok, connections} = Connection.connections()
    assert length(connections) == 2
    assert socket != socket2

    :ok = Connection.return(socket)
    {:ok, connections} = Connection.connections()
    assert length(connections) == 2

    {:ok, socket3} = Connection.acquire(node2(), self(), [{:active, :true}, {:packet, 4}])
    # Reuse check
    #assert socket == socket3

    :ok = Connection.close(socket3)
    :ok = Connection.close(socket)
    {:ok, connections} = Connection.connections()
    assert length(connections) == 1

    # # send and recieve at different processes
    spawn_link(__MODULE__, :test_api_send, [self()])
    assert (receive do
             :ok -> true
             _ -> false
            after 5000 -> false
    end)

    spawn_link(__MODULE__, :test_api_send, [self()])
    assert (receive do
             :ok -> true
             _ -> false
            after 5000 -> false
    end)

    Config.stop()
    Connection.stop()
  end

  # test "checking acquire/2 & acquire/4..." do
  #   {:ok, []} = Connection.connections()
  #   {:ok, socket} = Connection.acquire(ArtifactTest.node_beta, self())
  #   assert is_port(socket)

  #   :gen_tcp.send(socket, :erlang.term_to_binary(:ok))
  #   :ok = receive do
  #     {:tcp, _, bin} -> :erlang.binary_to_term(bin)
  #   end
  #   {:ok, socket2} = Connection.acquire(ArtifactTest.node_beta, self(), [active: true, packet: 4])
  #   refute socket == socket2

  #   {:ok, [_, _]} = Connection.connections()
  # end

end
