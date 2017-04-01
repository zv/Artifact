defmodule Artifact.Mixfile do
  use Mix.Project


  def project do
    [app: :artifact,
     version: "0.0.1",
     # elixir: "1.2.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
     package: package(),
    ]
  end

  defp package do
    %{licenses: ["MIT"],
      contributors: ["Zephyr Pellerin"],
      links: %{"Github" => "https://github.com/zv/artifact"}}
  end

  def application do
    [extra_applications: [:ranch]]
  end

  defp deps do
    [
      {:vclock, path: "vclock/"},
      {:ranch, "~> 1.2"},
    ]
  end
end

