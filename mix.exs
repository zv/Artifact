defmodule ArtifactElixir.Mixfile do
  use Mix.Project

  def project do
    [ app: :artifact,
      version: "0.0.1",
      elixir: "~> 0.9.4-dev",
      compile_path: "ebin",
      deps: deps ]
  end

  def application do
    []
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
