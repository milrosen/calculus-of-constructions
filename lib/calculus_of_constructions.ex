defmodule CalculusOfConstructions do
  @moduledoc """
  Documentation for `CalculusOfConstructions`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> CalculusOfConstructions.hello()
      :world

  """
  require(Core)
  require(Desugar)
  require(PrettyPrint)

  @type command :: :def | :check | :eval
  @type sentence(etype) :: {command, atom, etype}
  @type program(etype) :: [sentence(etype)]

  def hello do
    :world
  end

  def check(prog) do
    with(
      {:ok, tokens} <- tok(prog |> to_charlist),
      {:ok, program} <- parse(tokens)
    ) do
      {:ok, program |> desugar_program |> handle_commands(Map.new())}
    else
      {:TokenizerError, errmsg} ->
        {:error, "Tokenizer Error: #{errmsg}"}

      {:ParseError, errmsg} ->
        {:error, "Parse Error: #{errmsg}"}
    end
  end

  @spec handle_commands(program(Core.expr()), Desugar.context()) :: [{command(), any}]
  defp handle_commands([], _), do: []

  defp handle_commands([{:def, name, expr} | rst], ctx) do
    [{:def, name, expr} | handle_commands(rst, Map.put(ctx, name, expr))]
  end

  defp handle_commands([{:check, _, expr} | rst], ctx) do
    {:ok, subst} = Desugar.delta(ctx, expr)

    [
      case Core.typeOf(subst) do
        {:ok, ty} ->
          {:check, ty}

        {:UnboundVariableError, [{:var, {:v, name, _}}, _]} ->
          {:check, "Unbound Variable #{name} in #{PrettyPrint.printExpr(subst)} "}

        {:TypeMismatch, [t1, :DNE, t2]} ->
          {:check,
           "Expected argument of form #{PrettyPrint.printExpr(t1)} but recieved #{PrettyPrint.printExpr(t2)}"}
      end
      | handle_commands(rst, ctx)
    ]
  end

  defp handle_commands([{:eval, _, expr} | rst], ctx) do
    {:ok, subst} = Desugar.delta(ctx, expr)

    case Core.typeOf(subst) do
      {:ok, ty} ->
        {:eval, Core.normalize(ty)}

      {_, _} ->
        {:eval,
         "Could not eval term #{PrettyPrint.printExpr(subst)} since it failed to typecheck"}
    end
  end

  defp desugar_program([]), do: []

  defp desugar_program([{command, args, ast} | rst]) do
    [
      case desugar(ast) do
        {:ok, ast} -> {command, args, ast}
        {:LetError, errormsg} -> {command, args, errormsg}
      end
      | desugar_program(rst)
    ]
  end

  defp tok(prog) do
    case :lexer.string(prog) do
      {:ok, tokens, _} -> {:ok, tokens}
      {:error, errmsg, _} -> {:TokenizerError, errmsg}
    end
  end

  defp parse(tokens) do
    case :parser.parse(tokens) do
      {:ok, ast} -> {:ok, ast}
      {:error, errmsg} -> {:ParseError, errmsg}
    end
  end

  defp desugar(ast) do
    case ast |> Desugar.curry() |> (&Desugar.delta(Map.new(), &1)).() do
      {:ok, suggar_free} -> {:ok, suggar_free}
      {:error, errmsg} -> {:LetError, errmsg}
    end
  end

  @spec check_program(program(Core.expr())) :: [{command, Core.expr()}]
  def check_program([]), do: []
end
