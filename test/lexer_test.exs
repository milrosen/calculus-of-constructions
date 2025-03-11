defmodule LexerTests do
  use ExUnit.Case

  test "tokenizes valid string" do
    {:ok, tokens, _} = :lexer.string(~c"\\( a : * ) -> \\( x : a ) -> x")

    assert tokens == [
             {:lambda, 1},
             {:"(", 1},
             {:label, 1, :a},
             {:":", 1},
             {:star, 1},
             {:")", 1},
             {:arrow, 1},
             {:lambda, 1},
             {:"(", 1},
             {:label, 1, :x},
             {:":", 1},
             {:label, 1, :a},
             {:")", 1},
             {:arrow, 1},
             {:label, 1, :x}
           ]
  end

  test "comments" do
    {:ok, tokens, _} = :lexer.string(~c"(f : \\\/(_ : A) -> B) -- same thing as f : A -> B")

    assert tokens == [
             {:"(", 1},
             {:label, 1, :f},
             {:":", 1},
             {:pi, 1},
             {:"(", 1},
             {:label, 1, :_},
             {:":", 1},
             {:label, 1, :A},
             {:")", 1},
             {:arrow, 1},
             {:label, 1, :B},
             {:")", 1}
           ]
  end

  test "errors reported" do
    {:error, errors, _} = :lexer.string(~c"!")
    assert errors == {1, :lexer, {:illegal, ~c"!"}}
  end

  test "let" do
    # ((\id : A->A) -> C) (\(x:A)->x)
    {:ok, tokens, _} = :lexer.string(~c"let (id : A->A) = (\\(x:A) -> x) in C")

    assert tokens == [
             {:let, 1},
             {:"(", 1},
             {:label, 1, :id},
             {:":", 1},
             {:label, 1, :A},
             {:arrow, 1},
             {:label, 1, :A},
             {:")", 1},
             {:=, 1},
             {:"(", 1},
             {:lambda, 1},
             {:"(", 1},
             {:label, 1, :x},
             {:":", 1},
             {:label, 1, :A},
             {:")", 1},
             {:arrow, 1},
             {:label, 1, :x},
             {:")", 1},
             {:in, 1},
             {:label, 1, :C}
           ]
  end

  test "var with number" do
    {:ok, tokens, _} = :lexer.string(~c"t1")
    assert tokens == [{:label, 1, :t1}]
  end

  test "fun syntax" do
    {:ok, tokens, _} = :lexer.string(~c"fun x : A . x")

    assert tokens == [
             {:fun, 1},
             {:label, 1, :x},
             {:":", 1},
             {:label, 1, :A},
             {:dot, 1},
             {:label, 1, :x}
           ]
  end

  test "forall logical notation" do
    {:ok, tokens, _} = :lexer.string(~c"forall a : A, a")

    assert tokens == [
             {:forall, 1},
             {:label, 1, :a},
             {:":", 1},
             {:label, 1, :A},
             {:comma, 1},
             {:label, 1, :a}
           ]
  end

  test "forall curly notation" do
    {:ok, tokens, _} = :lexer.string(~c"\{a : A\} B")

    assert tokens == [
             {:"{", 1},
             {:label, 1, :a},
             {:":", 1},
             {:label, 1, :A},
             {:"}", 1},
             {:label, 1, :B}
           ]
  end

  test "program as list of commands" do
    {:ok, tokens, _} = :lexer.string(~c"#check let a = b in c")

    assert tokens ==
             [
               {:"#", 1},
               {:label, 1, :check},
               {:let, 1},
               {:label, 1, :a},
               {:=, 1},
               {:label, 1, :b},
               {:in, 1},
               {:label, 1, :c}
             ]
  end

  test "digits" do
    {:ok, tokens, _} = :lexer.string(~c"75")

    assert tokens == [{:number, 1, 75}]
  end
end
