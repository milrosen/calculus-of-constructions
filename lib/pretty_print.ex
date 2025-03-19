defmodule PrettyPrint do
  require(Core)
  @type expr :: Core.expr()

  @spec printExpr(expr) :: String
  def printExpr({:const, :box}), do: "□"
  def printExpr({:const, :star}), do: "*"
  def printExpr({:const, atom}), do: "#{atom}"

  def printExpr({:var, {:v, name, n}}) do
    if n == 0, do: "#{name}", else: "#{name}@#{n}"
  end

  def printExpr({:lam, name, e1, e2}) do
    "λ(#{name} : #{printExpr(e1)}) . #{printExpr(e2)}"
  end

  def printExpr({:pi, :_, {:pi, :_, e1, e2}, e3}) do
    "(#{printExpr(e1)} → #{printExpr(e2)}) → #{printExpr(e3)}"
  end

  def printExpr({:pi, :_, e1, e2}) do
    "#{printExpr(e1)} → #{printExpr(e2)}"
  end

  def printExpr({:pi, name, e1, e2}) do
    if not Core.free?({:v, name, 0}, e2),
      do: "(#{printExpr(e1)}) → #{printExpr(e2)}",
      else: "Π[#{name} : #{printExpr(e1)}] #{printExpr(e2)}"
  end

  def printExpr({:app, {:app, e1, e2}, e3}) do
    p2 =
      if match?({:var, _}, e2),
        do: PrettyPrint.printExpr(e2),
        else: "(#{PrettyPrint.printExpr(e2)})"

    p3 =
      if match?({:var, _}, e3),
        do: PrettyPrint.printExpr(e3),
        else: "(#{PrettyPrint.printExpr(e3)})"

    "#{printExpr(e1)} #{p2} #{p3}"
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
