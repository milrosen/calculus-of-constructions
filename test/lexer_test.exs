defmodule LexerTests do
  use ExUnit.Case

  test "tokenizes valid string" do
    {:ok, tokens, _} = :lexer.string('\\( a : * ) -> \\( x : a ) -> x')
    assert tokens == [{:lambda, 1}, {:rparen, 1}, {:label, 1, ~c"a"}, {:colon, 1}, {:star, 1}, {:lparen, 1}, {:arrow, 1}, {:lambda, 1}, {:rparen, 1}, {:label, 1, ~c"x"}, {:colon, 1}, {:label, 1, ~c"a"}, {:lparen, 1}, {:arrow, 1}, {:label, 1, ~c"x"}]
  end

  test "comments" do
    {:ok, tokens, _} = :lexer.string('(f : \\\/(_ : A) -> B) -- same thing as f : A -> B')
    assert tokens == [{:rparen, 1}, {:label, 1, ~c"f"}, {:colon, 1}, {:pi, 1}, {:rparen, 1}, {:label, 1, ~c"_"}, {:colon, 1}, {:label, 1, ~c"A"}, {:lparen, 1}, {:arrow, 1}, {:label, 1, ~c"B"}, {:lparen, 1}]
  end

  test "errors reported" do
    {:error, errors, _} = :lexer.string('#')
    assert errors == {1, :lexer, {:illegal, ~c"#"}}
  end
end
