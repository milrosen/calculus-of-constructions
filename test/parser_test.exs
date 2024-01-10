defmodule ParserTests do
  use ExUnit.Case

  test "parses valid string" do
    {:ok, tokens, _} = :lexer.string('(\\(a : *) -> \\(x : a) -> x) tree')
    {:ok, ast} = :parser.parse(tokens)
    assert ast ==
      {:app,
        {:lam,
          ~c"a",
          {:const, {:star, 1}},
          {:lam,
            ~c"x",
            {:var, {:v, ~c"a", 0}},
            {:var, {:v, ~c"x", 0}}
          }
        },
      {:var, {:v, ~c"tree", 0}}}
  end
end
