defmodule Artifact.Mixfile do
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

  defp deps do
    [ 
      # Exlager is a wrapper of the classic logging library from Basho
      { :exlager, ref: "f8dd0715d7668290d7d02a4517acfee4e8be885c", github: "khia/exlager" } 
    ]
  end
end
