defmodule CalculusOfInductiveTypes do
  @spec lex(binary) :: list
  def lex(str) do
    {:ok, tokens, _} = str |> String.to_charlist() |> :lexer.string()
    tokens
  end

  @spec parse(binary) :: list
  def parse(str) do
    {:ok, tokens, _} = str |> String.to_charlist() |> :lexer.string()
    {:ok, ast} = :parser.parse(tokens)
    ast
  end
end
