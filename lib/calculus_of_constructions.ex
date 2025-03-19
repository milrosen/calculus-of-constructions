defmodule CalculusOfConstructions do
  @moduledoc """
  Documentation for `CalculusOfConstructions`.
  """

  require(Core)
  require(Desugar)
  require(PrettyPrint)

  @type command :: :def | :typeof | :eval | :with | :check | :system
  @type sentence(etype) :: {command, atom, etype} | {:with, [{atom, etype}]}
  @type program(etype) :: [sentence(etype)]

  def check(prog) do
    with(
      {:ok, tokens} <- prog |> to_charlist |> tok,
      {:ok, program} <- parse(tokens)
    ) do
      {:ok,
       program
       |> desugar_program
       |> handle_commands(Map.new(), Map.new(), Core.get_coc_ax(), Core.get_coc_rules())}
    else
      {:TokenizerError, {line, :lexer, {:illegal, char}}} ->
        {:error, "Tokenizer Error on line #{line}: illegal char #{char}"}

      {:ParseError, {line, :parser, errmsg}} ->
        {:error, "Parse Error on line #{line}: #{Enum.join(errmsg, " ")}"}
    end
  end

  @spec handle_commands(
          program(Core.expr()),
          Desugar.context(),
          Core.context(),
          any,
          any
        ) :: [
          {command(), any}
        ]
  defp handle_commands([], _, _, _, _), do: []

  defp handle_commands([{:def, name, expr} | rst], terms_ctx, types_ctx, axioms, rules) do
    {:ok, subst} = Desugar.delta(terms_ctx, expr)

    [
      {:def, name, expr}
      | handle_commands(rst, Map.put(terms_ctx, name, subst), types_ctx, axioms, rules)
    ]
  end

  defp handle_commands([{:with, annos} | rst], terms_ctx, types_ctx, axioms, rules) do
    types_ctx =
      Map.merge(
        Map.new(annos, fn {name, type} ->
          {{:v, name, 0},
           with({:ok, term} <- type |> (&Desugar.delta(terms_ctx, &1)).(), do: term)}
        end),
        types_ctx
      )

    [{:with, types_ctx} | handle_commands(rst, terms_ctx, types_ctx, axioms, rules)]
  end

  defp handle_commands([{:system, sorts, axioms, rules} | rst], terms_ctx, types_ctx, _, _) do
    sorts =
      Enum.map(sorts, fn sort -> %{sort => {:const, sort}} end)
      |> Enum.reduce(&Map.merge/2)

    axioms =
      Enum.map(axioms, fn {left, right} -> %{left => right} end)
      |> Enum.reduce(&Map.merge/2)

    rules =
      Enum.map(rules, fn {s1, s2, s3} -> %{{s1, s2} => s3} end)
      |> Enum.reduce(&Map.merge/2)

    terms_ctx = Map.merge(sorts, terms_ctx)

    [{:system, sorts, axioms, rules} | handle_commands(rst, terms_ctx, types_ctx, axioms, rules)]
  end

  defp handle_commands([{:check, name, type, term} | rst], terms_ctx, types_ctx, axioms, rules) do
    {:ok, term} = Desugar.delta(terms_ctx, term)
    {:ok, type} = Desugar.delta(terms_ctx, type)

    [
      case Core.typeOf(term, types_ctx, axioms, rules) do
        {:ok, ty} ->
          if Core.eq(ty, type),
            do: {:check, name, Core.normalize(type), Core.normalize(term)},
            else:
              {:error,
               "#{PrettyPrint.printExpr(Core.normalize(ty))} is not equal to #{PrettyPrint.printExpr(Core.normalize(type))}"}

        err ->
          handle_error(err)
      end
      | handle_commands(rst, Map.put(terms_ctx, name, term), types_ctx, axioms, rules)
    ]
  end

  defp handle_error(error) do
    case error do
      {:UnboundVariableError, [{:var, {:v, name, _}}, _]} ->
        {:error, "Unbound Variable #{name}"}

      {:TypeMismatch, [t1, :DNE, t2, _]} ->
        {:error,
         "Expected argument of form #{PrettyPrint.printExpr(t1)} but recieved #{PrettyPrint.printExpr(t2)}"}

      {:NotAFunction, [term, type]} ->
        {:error,
         "#{PrettyPrint.printExpr(term)} : #{PrettyPrint.printExpr(type)} is not a function"}

      {:InvalidQuantification, term} ->
        {:error, "attempted to create dependency: #{PrettyPrint.printExpr(term)}"}
    end
  end

  defp handle_commands([{:typeof, _, expr} | rst], terms_ctx, types_ctx, axioms, rules) do
    {:ok, subst} = Desugar.delta(terms_ctx, expr)

    [
      case Core.typeOf(subst, types_ctx, axioms, rules) do
        {:ok, ty} ->
          {:typeof, ty}

        err ->
          handle_error(err)
      end
      | handle_commands(rst, terms_ctx, types_ctx, axioms, rules)
    ]
  end

  defp handle_commands([{:eval, _, expr} | rst], terms_ctx, types_ctx, axioms, rules) do
    {:ok, subst} = Desugar.delta(terms_ctx, expr)

    [
      case Core.typeOf(subst, types_ctx, axioms, rules) do
        {:ok, ty} ->
          {:eval, Core.normalize(subst), Core.normalize(ty)}

        {_, _} ->
          {:error,
           "Could not eval term #{PrettyPrint.printExpr(subst)} since it failed to typecheck"}
      end
      | handle_commands(rst, terms_ctx, types_ctx, axioms, rules)
    ]
  end

  defp desugar_program([]), do: []

  defp desugar_program([{:with, annos} | rst]) do
    desugared =
      Enum.map(annos, fn {name, sterm} ->
        case desugar(sterm) do
          {:ok, term} -> {name, term}
          {:LetError, _} -> {name, {:const, :star}}
        end
      end)

    [{:with, desugared} | desugar_program(rst)]
  end

  defp desugar_program([{:system, sorts, axioms, rules} | rst]) do
    [{:system, sorts, axioms, rules} | desugar_program(rst)]
  end

  defp desugar_program([{:check, name, type, term} | rst]) do
    [
      with(
        {:ok, type} <- desugar(type),
        {:ok, term} <- desugar(term)
      ) do
        {:check, name, type, term}
      end
      | desugar_program(rst)
    ]
  end

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
