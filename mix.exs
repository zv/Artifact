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

  # defp description do
  # """
  # A distributed database inspired by Chord with a wide array of features from
  # the Dynamo Paper, the XZZ Paper and more
  # """
  # end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [{:vclock, path: "vclock/"}]
  end
end

