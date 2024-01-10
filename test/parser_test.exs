defmodule ParserTests do
  use ExUnit.Case

  test "parses valid string" do
    {:ok, tokens, _} = :lexer.string('(\\(a : *) -> \\(x : a) -> x) tree')
    {:ok, ast} = :parser.parse(tokens)
    assert ast ==
      {
      :app,
      {
        :lam,
        {:label, 1, ~c"a"},
        {:const, {:star, 1}},
        {:lam, {:label, 1, ~c"x"}, {:var, {:v, {:label, 1, ~c"a"}, 0}}, {:var, {:v, {:label, 1, ~c"x"}, 0}}}
      },
      {:var, {:v, {:label, 1, ~c"tree"}, 0}}
    }
  end
end
