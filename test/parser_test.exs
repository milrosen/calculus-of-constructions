defmodule ParserTests do
  use ExUnit.Case

  test "parses valid string" do
    {:ok, tokens, _} = :lexer.string('(\\(a : *) -> \\(x : a) -> x) tree')
    {:ok, ast} = :parser.parse(tokens)
    assert ast ==
      {:app,
        {:lam,
          :a,
          {:const, :star},
          {:lam,
            :x,
            {:var, {:v, :a, 0}},
            {:var, {:v, :x, 0}}
          }
        },
      {:var, {:v, :tree, 0}}}
  end

  test "@ sign" do
    {:ok, tokens, _} = :lexer.string('x@1')
    {:ok, ast} = :parser.parse(tokens)
    assert ast == {:var, {:v, :x, 1}}
  end

  test "unmatched parens" do
    {:ok, tokens, _} = :lexer.string('\\(a : a) -> \\(x : a -> x \n (x)')
    {:error, errors} = :parser.parse(tokens)
    assert errors
  end

  test "unmatched parens with comment" do
    {:ok, tokens, _} = :lexer.string('\\(a : a) -> \\(x : a --hello world ) -> x')
    {:error, errors} = :parser.parse(tokens)
    assert errors
  end
end
