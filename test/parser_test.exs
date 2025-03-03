defmodule ParserTests do
  use ExUnit.Case

  test "parses valid string" do
    {:ok, tokens, _} = :lexer.string(~c"(\\(a : *) -> \\(x : a) -> x) tree")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:app,
              {:lam, [{:a, {:const, :star}}],
               {:lam, [{:x, {:var, {:v, :a, 0}}}], {:var, {:v, :x, 0}}}}, {:var, {:v, :tree, 0}}}
  end

  test "@ sign" do
    {:ok, tokens, _} = :lexer.string(~c"x@1")
    {:ok, ast} = :parser.parse(tokens)
    assert ast == {:var, {:v, :x, 1}}
  end

  test "product type" do
    {:ok, tokens, _} = :lexer.string(~c"\\\/ (a : A) -> B")
    {:ok, ast} = :parser.parse(tokens)
    assert ast == {:pi, [{:a, {:var, {:v, :A, 0}}}], {:var, {:v, :B, 0}}}
  end

  # test "implicit star type" do
  #   {:ok, tokens, _} = :lexer.string(~c"\\x -> x")
  #   {:ok, ast} = :parser.parse(tokens)
  #   assert ast == {:lam, :x, {:const, :star}, {:var, {:v, :x, 0}}}
  # end

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

  test "let expr" do
    {:ok, tokens, _} = :lexer.string(~c"let a : A = x in a")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:let, :a, {:var, {:v, :A, 0}}, {:var, {:v, :x, 0}}, {:var, {:v, :a, 0}}}
  end

  test "fun type dot notation" do
    {:ok, tokens, _} = :lexer.string(~c"fun x : a . x")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:lam, [{:x, {:var, {:v, :a, 0}}}], {:var, {:v, :x, 0}}}
  end

  test "forall logical notation" do
    {:ok, tokens, _} = :lexer.string(~c"forall a : A, B")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:pi, [{:a, {:var, {:v, :A, 0}}}], {:var, {:v, :B, 0}}}
  end

  test "forall curly notation" do
    {:ok, tokens, _} = :lexer.string(~c"{a : A} B")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:pi, [{:a, {:var, {:v, :A, 0}}}], {:var, {:v, :B, 0}}}
  end

  # harder
  test "multiple arguments" do
    {:ok, tokens, _} = :lexer.string(~c"fun x : A, y : A . x")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:lam, [{:x, {:var, {:v, :A, 0}}}, {:y, {:var, {:v, :A, 0}}}], {:var, {:v, :x, 0}}}
  end

  test "mixed curry" do
    {:ok, tokens, _} = :lexer.string(~c"{M : *} fun x : *, y : A . forall y : A, x")
    {:ok, ast} = :parser.parse(tokens)

    assert ast ==
             {:pi, [M: {:const, :star}],
              {:lam, [x: {:const, :star}, y: {:var, {:v, :A, 0}}],
               {:pi, [y: {:var, {:v, :A, 0}}], {:var, {:v, :x, 0}}}}}
  end
end
