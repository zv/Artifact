defmodule Artifact do
  @moduledoc """
  Artifact is a performant, scalable distributed key-value store system,
  whose intellectual ancestor is Amazon's Dynamo database model, but serves as
  one of (if not the only) implementation of the SILT database concept.

  It's open source and freely available on github. Unstructured and permissive
  in what it accepts, strict in what it delivers -- perfect for storing large
  integers of small (<128k) data records which need to be read or processed in a
  realtime, low-latency environment.

  This is the main module in the Artifact repository.
  """
  use Behaviour
  require Record

  @typedoc """
  This is a struct used to represent a piece of keyed data stored by an
  Artifact node
  """
  @type vclock_node :: term
  @type timestamp   :: integer
  @type counter     :: integer
  @type dot         :: {vclock_node, {counter, timestamp}}
  @type vclock      :: [dot]
  @type data :: record(:data,
                       key:           bitstring,
                       bucket:        integer,
                       last_modified: integer,
                       vector_clocks: vclock,
                       checksum:      binary,
                       flags:         bitstring,
                       value:         binary)
  # A record stores metadata about a particular value and where it is stored.
  Record.defrecord :data, [key: nil, bucket: nil, last_modified: nil,
                           vector_clocks: nil, checksum: nil, flags: nil, value: nil]

  def start(_type , _args), do: Artifact.Supervisor.start_link()
end
