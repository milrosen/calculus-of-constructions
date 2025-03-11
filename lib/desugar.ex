defmodule Desugar do
  require Core

  @type anno :: {atom, cexpr()}

  @type cexpr ::
          {:const, :star}
          | {:const, :box}
          | {:const, {:number, integer()}}
          | {:var, {:v, atom, integer()}}
          | {:lam, [anno()], cexpr()}
          | {:pi, [anno()], cexpr()}
          | {:let, atom, cexpr(), cexpr(), cexpr()}
          | {:app, cexpr(), cexpr()}

  @type lexpr ::
          {:const, :star}
          | {:const, :box}
          | {:const, {:number, integer}}
          | {:var, {:v, atom, integer}}
          | {:lam, atom, lexpr, lexpr}
          | {:pi, atom, lexpr, lexpr}
          | {:let, atom, lexpr, lexpr, lexpr}
          | {:app, lexpr, lexpr}

  @spec curry(cexpr) :: Core.expr()
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

  def curry({:let, x, t, t1, t2}), do: {:let, x, curry(t), curry(t1), curry(t2)}
  def curry({:app, t1, t2}), do: {:app, curry(t1), curry(t2)}

  @type context :: %{atom => Core.expr()}
  @spec delta(context, lexpr()) :: {:ok, Core.expr()} | {:error, any}
  def delta(_, {:const, {:number, 0}}), do: {:ok, {:var, {:v, :zero, 0}}}

  def delta(ctx, {:const, {:number, n}}) do
    with {:ok, n1} <- delta(ctx, {:const, {:number, n - 1}}) do
      {:ok, {:app, {:var, {:v, :succ, 0}}, n1}}
    end
  end

  def delta(_, {:const, c}), do: {:ok, {:const, c}}

  def delta(ctx, {:var, {:v, name, _}} = v) do
    case ctx do
      %{^name => e} -> {:ok, e}
      _ -> {:ok, v}
    end
  end

  def delta(ctx, {:let, name, _, t1, t2}) do
    case ctx do
      %{^name => _} ->
        {:error, "attempt to redefine already bound term #{name}"}

      _ ->
        delta(Map.put(ctx, name, t1), t2)
    end
  end

  def delta(ctx, {:app, t1, t2}) do
    with(
      {:ok, t1} <- delta(ctx, t1),
      {:ok, t2} <- delta(ctx, t2)
    ) do
      {:ok, {:app, t1, t2}}
    end
  end

  def delta(ctx, {:lam, name, t1, t2}) do
    if Map.has_key?(ctx, name) do
      {:error, "redefining bound term #{name}"}
    else
      with(
        {:ok, t1} <- delta(ctx, t1),
        {:ok, t2} <- delta(ctx, t2)
      ) do
        {:ok, {:lam, name, t1, t2}}
      end
    end
  end

  def delta(ctx, {:pi, name, t1, t2}) do
    if Map.has_key?(ctx, name) do
      {:error, "redefining bound term #{name}"}
    else
      with(
        {:ok, t1} <- delta(ctx, t1),
        {:ok, t2} <- delta(ctx, t2)
      ) do
        {:ok, {:pi, name, t1, t2}}
      end
    end
  end
end
