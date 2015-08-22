use Mix.Config

config :artifact, TCP,
  listen: [
    {:active, false},
    :binary,
    {:packet, :line},
    {:reuseaddr, true}
  ],
  port: 11211,
  max_processes: 8,
  max_restarts: 3,
  time: 60,
  shutdown: 2000,
  accept_timeout: :infinity,
  accept_error_sleep_time: 3000,
  recv_length: 0,
  recv_timeout: :infinity
