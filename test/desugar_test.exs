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

  test "numbers" do
    {:ok, tokens, _} = :lexer.string(~c"(+) 5 10")
    {:ok, [ast]} = :parser.parse(tokens)
    curried = Desugar.curry(ast)

    {:ok, tokens2, _} = :lexer.string(~c"(+) (succ (succ (succ (succ (succ zero))))) 10")
    {:ok, [ast2]} = :parser.parse(tokens2)
    curried2 = Desugar.curry(ast2)

    assert Desugar.delta(%{}, curried) == Desugar.delta(%{}, curried2)
  end
end
