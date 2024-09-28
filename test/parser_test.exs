defmodule ParserTests do
  use ExUnit.Case

  test "parses valid string" do
    {:ok, tokens, _} = :lexer.string(~c"(\\(a : *) -> \\(x : a) -> x) tree")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:app,
              {:lam, :a, {:const, :star}, {:lam, :x, {:var, {:v, :a, 0}}, {:var, {:v, :x, 0}}}},
              {:var, {:v, :tree, 0}}}
  end

  test "@ sign" do
    {:ok, tokens, _} = :lexer.string(~c"x@1")
    {:ok, ast} = :parser.parse(tokens)
    assert ast == {:var, {:v, :x, 1}}
  end

  test "product type" do
    {:ok, tokens, _} = :lexer.string(~c"\\\/ (a : A) -> B")
    {:ok, ast} = :parser.parse(tokens)
    assert ast == {:pi, :a, {:var, {:v, :A, 0}}, {:var, {:v, :B, 0}}}
  end

  test "unmatched parens" do
    {:ok, tokens, _} = :lexer.string(~c"\\(a : a) -> \\(x : a -> x \n (x)")
    {:error, errors} = :parser.parse(tokens)
    assert errors
  end

  test "unmatched parens with comment" do
    {:ok, tokens, _} = :lexer.string(~c"\\(a : a) -> \\(x : a --hello world ) -> x")
    {:error, errors} = :parser.parse(tokens)
    assert errors
  end

  test "let" do
    # ((\id : A->A) -> C) (\(x:A)->x)
    {:ok, tokens, _} = :lexer.string(~c"let (id : A->A) = (\\(x:A) -> x) in C")
    {:ok, let} = :parser.parse(tokens)
    {:ok, tokens, _} = :lexer.string(~c"(\\(id : A -> A) -> C) (\\(x:A) -> x)")
    {:ok, default} = :parser.parse(tokens)
    assert let == default
  end

  test "more complex let" do
    {:ok, tokens, _} =
      :lexer.string(
        ~c"let (th1 : (forall(P : *) -> forall(Q : *) -> P -> Q -> P)) =
                                         \\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp in C"
      )

    {:ok, let} = :parser.parse(tokens)

    {:ok, tokens, _} =
      :lexer.string(
        ~c"(\\(th1 : (forall(P : *) -> forall(Q : *) -> P -> Q -> P)) -> C)
                                          (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp)"
      )

    {:ok, default} = :parser.parse(tokens)
    assert let == default
  end

  test "nested let" do
    {:ok, tokens, _} = :lexer.string(~c"\\(P : *) -> \\(Q : *) ->
                                      let (th1 : *) =
                                      \\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp in
                                      let (th2 : *) =
                                      \\(P : *) -> \\(F : P -> Q) -> \\(hp : *) -> F hp in
                                      C")
    {:ok, let} = :parser.parse(tokens)

    {:ok, tokens, _} =
      :lexer.string(
        ~c"\\(P : *) -> \\(Q : *) ->
                                          ((\\(th1 : *) -> (\\(th2 : *) -> C)
                                          (\\(P : *) -> \\(F : P -> Q) -> \\(hp : *) -> F hp)))
                                          (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp)"
      )

    {:ok, default} = :parser.parse(tokens)
    assert PrettyPrint.printExpr(let) == PrettyPrint.printExpr(default)

    assert let == default
  end
end
