defmodule DesugarTests do
  use ExUnit.Case

  test "curry fun" do
    {:ok, tokens, _} = :lexer.string(~c"fun x : A, y : A . x")
    {:ok, ast} = :parser.parse(tokens)

    assert Desugar.curry(ast) ==
             {:lam, :x, {:var, {:v, :A, 0}}, {:lam, :y, {:var, {:v, :A, 0}}, {:var, {:v, :x, 0}}}}
  end

  test "let subst" do
    {:ok, tokens, _} = :lexer.string(~c"let bot : * = {A : *} A in A -> bot")
    {:ok, ast} = :parser.parse(tokens)

    assert Desuger.delta(ast) ==
             {:pi, :_, {:var, {:v, :A, 0}}, {:pi, :A, {:var, {:v, :A, 0}}}}
  end
end
