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

  test "product type" do
    {:ok, tokens, _} = :lexer.string('\\\/ (a : A) -> B')
    {:ok, ast} = :parser.parse(tokens)
    assert ast == {:pi, :a, {:var, {:v, :A, 0}}, {:var, {:v, :B, 0}}}
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

  test "let" do
    {:ok, tokens, _} = :lexer.string('let (id : A->A) = (\\(x:A) -> x) in C') # ((\id : A->A) -> C) (\(x:A)->x)
    {:ok, let} = :parser.parse(tokens)
    {:ok, tokens, _} = :lexer.string('(\\(id : A -> A) -> C) (\\(x:A) -> x)')
    {:ok, default} = :parser.parse(tokens)
    assert let == default
  end

  test "more complex let" do
    {:ok, tokens, _} = :lexer.string('let (th1 : (forall(P : *) -> forall(Q : *) -> P -> Q -> P)) =
                                         \\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp in C')
    {:ok, let} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('(\\(th1 : (forall(P : *) -> forall(Q : *) -> P -> Q -> P)) -> C)
                                          (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp)')
    {:ok, default} = :parser.parse(tokens)
    assert let == default
  end

  test "nested let" do
    {:ok, tokens, _} = :lexer.string('\\(P : *) -> \\(Q : *) ->
                                      let (th1 : (forall(P : *) -> forall(Q : *) -> P -> Q -> P)) =
                                      \\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp in C')
    {:ok, let} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(P : *) -> \\(Q : *) ->
                                          (\\(th1 : (forall(P : *) -> forall(Q : *) -> P -> Q -> P)) -> C)
                                          (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp)')

    {:ok, default} = :parser.parse(tokens)
    assert let == default
  end
end
