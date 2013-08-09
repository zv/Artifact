defmodule Artifact.Mixfile do
  use Mix.Project

  def project do
    [ app: :artifact,
      version: "0.0.1",
      elixir: "~> 0.10.0",
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    [ 
      # Exlager is a wrapper of the classic logging library from Basho
      { :exlager, ref: "b50e47abe39b2cb28fa431f0af4390c2324bf229", github: "khia/exlager"}
    ]
  end
end
