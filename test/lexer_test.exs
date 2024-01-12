defmodule LexerTests do
  use ExUnit.Case

  test "tokenizes valid string" do
    {:ok, tokens, _} = :lexer.string('\\( a : * ) -> \\( x : a ) -> x')
    assert tokens == [{:lambda, 1}, {:"(", 1}, {:label, 1, :a}, {:":", 1}, {:star, 1},  {:")", 1}, {:arrow, 1}, {:lambda, 1}, {:"(", 1}, {:label, 1, :x}, {:":", 1}, {:label, 1, :a}, {:")", 1}, {:arrow, 1}, {:label, 1, :x}]
  end

  test "comments" do
    {:ok, tokens, _} = :lexer.string('(f : \\\/(_ : A) -> B) -- same thing as f : A -> B')
    assert tokens == [{:"(", 1}, {:label, 1, :f}, {:":", 1}, {:pi, 1}, {:"(", 1}, {:label, 1, :_}, {:":", 1}, {:label, 1, :A}, {:")", 1}, {:arrow, 1}, {:label, 1, :B}, {:")", 1}]
  end

  test "errors reported" do
    {:error, errors, _} = :lexer.string('#')
    assert errors == {1, :lexer, {:illegal, ~c"#"}}
  end
end
