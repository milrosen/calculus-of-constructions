defmodule Desugar do
  require(Core)
  @type anno :: {atom, cexpr()}

  @type cexpr ::
          {:const, :star}
          | {:const, :box}
          | {:var, {:v, atom, integer()}}
          | {:lam, [anno()], cexpr()}
          | {:pi, [anno()], cexpr()}
          | {:let, anno(), cexpr(), cexpr()}
          | {:app, cexpr(), cexpr()}

  @type lexpr :: Core.expr() | {:let, atom(), Core.expr(), lexpr()}

  @spec curry(cexpr()) :: lexpr()
  def curry({:const, c}), do: {:const, c}
  def curry({:var, v}), do: {:var, v}
  def curry({:lam, [{x, t1} | []], t2}), do: {:lam, x, curry(t1), curry(t2)}

  def curry({:lam, [{x, t1} | rst], t2}) do
    {:lam, x, curry(t1), curry({:lam, rst, t2})}
  end

  def curry({:pi, [{x, t1} | []], t2}), do: {:pi, x, curry(t1), curry(t2)}

  def curry({:pi, [{x, t1} | rst], t2}) do
    {:pi, x, curry(t1), curry({:pi, rst, t2})}
  end

  def curry({:a}), do: :a
end
