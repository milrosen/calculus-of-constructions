defmodule CalculusOfInductiveTypes do
  alias Core

  def typeProof(proofList) do
    astList =
      Enum.reduce_while(proofList, [], fn p, acc ->
        with {:ok, tokens, _} <- :lexer.string(~c"#{p}"),
             {:ok, ast} <- :parser.parse(tokens) do
          {:cont, [ast | acc]}
        else
          error -> {:halt, error}
        end
      end)
      |> Enum.reverse()

    case Core.typeList(astList) do
      {:ok, typeList} ->
        {:ok,
         Enum.map(typeList, fn {type, ctx} ->
           %{
             type: PrettyPrint.printExpr(type),
             context:
               Enum.map(ctx, fn {var, type} ->
                 pretty_var =
                   case var do
                     {:v, atom, 0} -> "#{atom}"
                     {:v, atom, n} -> "#{atom}@#{n}"
                   end

                 %{pretty_var => PrettyPrint.printExpr(type)}
               end)
           }
         end)}

      {:error, errorType, errorMsg} ->
        {:typeError, PrettyPrint.printError({:error, errorType, errorMsg})}
    end
  end
end
