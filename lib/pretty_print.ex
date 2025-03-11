defmodule PrettyPrint do
  require(Core)
  @type expr :: Core.expr()

  @spec printExpr(expr) :: String
  def printExpr({:const, :box}), do: "□"
  def printExpr({:const, :star}), do: "*"

  def printExpr({:var, {:v, name, n}}) do
    if n == 0, do: "#{name}", else: "#{name}@#{n}"
  end

  def printExpr({:lam, name, e1, e2}) do
    "λ(#{name} : #{printExpr(e1)}) → #{printExpr(e2)}"
  end

  def printExpr({:pi, :_, {:pi, :_, e1, e2}, e3}) do
    "(#{printExpr(e1)} → #{printExpr(e2)}) → #{printExpr(e3)}"
  end

  def printExpr({:pi, :_, e1, e2}) do
    "#{printExpr(e1)} → #{printExpr(e2)}"
  end

  def printExpr({:pi, name, e1, e2}) do
    "Π(#{name} : #{printExpr(e1)}) → #{printExpr(e2)}"
  end

  def printExpr({:app, e1, e2}) do
    "(#{printExpr(e1)} #{printExpr(e2)})"
  end

  defp printExprOrAtom(e) do
    if is_atom(e) do
      " #{e}: "
    else
      printExpr(e)
    end
  end

  def printError({:error, errortype, error}) do
    "#{errortype}: #{Enum.map(error, fn e -> printExprOrAtom(e) end)}"
  end
end
