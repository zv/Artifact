defmodule Artifact.Mixfile do
  use Mix.Project

  def project do
    [ app: :artifact,
      version: "0.0.1",
      elixir: "~> 0.11.0",
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    [
      # Exlager is a wrapper of the classic logging library from Basho
      { :exlager, ref: "0b840aae773e6eb6de7cdbb703b47031b6608bfb", github: "khia/exlager"}
    ]
  end
end
