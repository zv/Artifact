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

  use Behaviour

  @type vclock_node :: term
  @type timestamp   :: number
  @type counter     :: number
  @type dot         :: {vclock_node, {counter, timestamp}}
  @type vclock      :: [dot]

  defmodule Data do
    @moduledoc """
    This is a struct used to represent a piece of keyed data stored by an
    Artifact node
    """
    @type key           :: bitstring
    @type bucket        :: number
    @type last_modified :: number
    @type vector_clocks :: vclock
    @type checksum      :: binary
    @type flags         :: bitstring
    @type value         :: binary

    defstruct {
      key: key, bucket: bucket,
      last_modified: last_modified, vector_clocks: vector_clocks,
      checksum: checksum, flags: flags, value: value
    }
  end

  @doc """
  Logs an error

  ## Examples

      Artifact.error "oops"
  """
  defmacro error(message), do: log(:error, message)

  @doc """
  Logs a warning.

  ## Examples

      Artifact.warning "knob turned too far to the right"
  """
  defmacro warning(message), do: log(:warning, message)

  @doc """
  Logs an informational message.

  ## Examples

      Artifact.info "My name is artifact"
  """
  defmacro info(message), do: log(:info, message)

  @doc """
  Logs a debug message

  ## Examples

      Artifact.debug "error?"
  """
  defmacro debug(message), do: log(:debug, message)

  def start(_type , _args) do
    Artifact.Supervisor.start_link(args)
  end

end
