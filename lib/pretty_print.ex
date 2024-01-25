defmodule PrettyPrint do
  require(CalculusOfInductiveTypes)
  @type expr :: CalculusOfInductiveTypes.expr()

  @spec printExpr(expr()) :: String
  def printExpr({:const,  :box}), do: "□"
  def printExpr({:const, :star}), do: "*"
  def printExpr({:var, {:v, name, n}}) do
    if n == 0, do: name, else: name <> "@" <> n
  end
  def printExpr({:lam, name, e1, e2}) do
    "λ(#{name} : #{printExpr(e1)}) -> #{printExpr(e2)}"
  end

  def printExpr({:pi, :_, e1, e2}) do
    "#{printExpr(e1)} -> #{printExpr(e2)}"
  end
  def printExpr({:pi, name, e1, e2}) do
    "Π(#{name} : #{printExpr(e1)}) -> #{printExpr(e2)}"
  end

  def printExpr({:app, e1, e2}) do
    "#{printExpr(e1)} #{printExpr(e2)}"
  end

end
