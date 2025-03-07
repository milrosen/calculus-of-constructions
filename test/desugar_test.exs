defmodule DesugarTests do
  use ExUnit.Case

  test "curry fun" do
    {:ok, tokens, _} = :lexer.string(~c"fun x : A, y : A . x")
    {:ok, [ast]} = :parser.parse(tokens)

    assert Desugar.curry(ast) ==
             {:lam, :x, {:var, {:v, :A, 0}}, {:lam, :y, {:var, {:v, :A, 0}}, {:var, {:v, :x, 0}}}}
  end

  test "let subst" do
    {:ok, tokens, _} = :lexer.string(~c"let bot : * = forall A:*, A in B -> bot")
    {:ok, [ast]} = :parser.parse(tokens)
    curried = Desugar.curry(ast)

    assert Desugar.delta(%{}, curried) ==
             {:ok,
              {:pi, :_, {:var, {:v, :B, 0}}, {:pi, :A, {:const, :star}, {:var, {:v, :A, 0}}}}}
  end
end
