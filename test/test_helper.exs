ExUnit.start


defmodule Artifact.TestMacros do
  defmacro node1 do
    quote do: {{127,0,0,1}, 11011}
  end

  defmacro node2 do
    quote do: ({{127,0,0,1}, 11012})
  end

  defmacro node3 do
    quote do: {{127,0,0,1}, 11013}
  end

  defmacro node4 do
    quote do: {{127,0,0,1}, 11014}
  end
end
