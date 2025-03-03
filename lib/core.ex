defmodule Core do
  @type const :: :star | :box
  @type error :: :TypeError

  @spec axiom(const) :: {:ok, const} | {:error, :TypeError}
  def axiom(:star), do: {:ok, :box}
  def axiom(:box), do: {:error, :TypeError}

  @spec rule(const, const) :: const

  # * ⤳ *, or terms can depend on terms. This rule is equivalent to the simply typed lambda calculus
  def rule(:star, :star), do: :star

  # □ ⤳ *, or terms can depend on types. This is eqivalent to the F2, where types and terms are fundamentally different
  def rule(:box, :star), do: :star

  # □ ⤳ □, or types can depend on types. This enables Meta types, like Tree<T> and so on
  def rule(:box, :box), do: :box

  # * ⤳ □, or types can depend on terms. This is sometimes called "dependent typing" and makes this the CoC
  def rule(:star, :box), do: :box

  @type expr ::
          {:const, const}
          | {:var, {:v, atom, integer}}
          | {:lam, atom, expr(), expr()}
          | {:pi, atom, expr(), expr()}
          | {:app, expr(), expr()}

  # adds n to each free x@n in e.
  @spec shift(integer, atom, expr) :: expr
  def shift(d, x0, e0) do
    # the @n indexes how nested the binding of X is. Following diagram from Morte.hs library
    # >                           +-refers to-+
    # >                           |           |
    # >                           v           |
    # > \(x : *) -> \(y : *) -> \(x : *) -> x@0
    # >
    # >   +-------------refers to-------------+
    # >   |                                   |
    # >   v                                   |
    # > \(x : *) -> \(y : *) -> \(x : *) -> x@1

    # Pi and Lambda cases count how many bound x's there are,
    # if we pass two x's, then we know x's index needs to be at least two to shift
    go = fn
      {:lam, x, a, b}, c, go ->
        {:lam, x, go.(a, c, go), go.(b, if(x == x0, do: c + 1, else: c), go)}

      {:pi, x, a, b}, c, go ->
        {:pi, x, go.(a, c, go), go.(b, if(x == x0, do: c + 1, else: c), go)}

      {:app, f, a}, c, go ->
        {:app, go.(f, c, go), go.(a, c, go)}

      {:var, {:v, x, n}}, c, _ ->
        {:var, {:v, x, if(x == x0 && n >= c, do: d + n, else: n)}}

      {:const, s}, _, _ ->
        {:const, s}
    end

    go.(e0, 0, go)
  end

  # replace every free x in B with C

  # Rule for application --- relavant cause that's where substituion occurs
  #  Γ⊢ f : (Πx : A. B)       Γ⊢ a : A
  #          Γ⊢ f a : B[x := a]
  #
  # read, with some f : A->B (where B could depend on x), and a : A,
  # f a is of type B[x := a]

  # note that the lambda and Pi rules are identical. Isn't that cool!!
  # this is because we are in the CoC, and the application rule is the same for types and terms,
  # where the type of a function is expressed as a dependent product, reguardless of what its abstracting

  # substitue(x n C B) -> B[x@n := C]
  @spec subst(atom, integer, expr, expr) :: expr
  def subst(x, n, e1, {:var, {:v, x1, n1}} = v),
    do: if(x1 == x && n1 == n, do: e1, else: v)

  def subst(_, _, _, {:const, const}),
    do: {:const, const}

  def subst(x, n, e1, {:app, f, a}),
    do: {:app, subst(x, n, e1, f), subst(x, n, e1, a)}

  def subst(x, n, e1, {:lam, y, a, b}) do
    n1 = if x == y, do: n + 1, else: n
    b1 = subst(x, n1, shift(1, y, e1), b)
    {:lam, y, subst(x, n, e1, a), b1}
  end

  def subst(x, n, e1, {:pi, y, a, b}) do
    n1 = if x == y, do: n + 1, else: n
    b1 = subst(x, n1, shift(1, y, e1), b)
    {:pi, y, subst(x, n, e1, a), b1}
  end

  # V free in e?
  @spec free?({:v, atom, integer}, expr) :: boolean
  def free?(
        {:v, _, _} = v,
        {:var, v1}
      ),
      do: v == v1

  def free?(
        {:v, _, _},
        {:const, _}
      ),
      do: false

  def free?(
        {:v, _, _} = v,
        {:app, f, a}
      ),
      do: free?(v, f) || free?(v, a)

  def free?(
        {:v, x, n} = v,
        {:lam, x1, a, b}
      ) do
    free?(v, a) || if x == x1, do: free?({:v, x, n + 1}, b), else: free?(v, b)
  end

  def free?(
        {:v, x, n} = v,
        {:pi, x1, a, b}
      ) do
    free?(v, a) || if x == x1, do: free?({:v, x, n + 1}, b), else: free?(v, b)
  end

  # weak-head-normal form. Basically, no more outside applications to make.
  # \x -> 1 + 2 is fine, since the pending application is inside of the function, but (\x -> 1) 2 isn't
  # more info here: https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form
  @spec whnf(expr) :: expr
  def whnf(e) when Kernel.elem(e, 0) != :app, do: e

  # beta reduction:
  # (\x:A -> x) ((\x:B -> x)   : A)
  # (\x:A -> x) ((\x:B -> x@1) : A) -- adds one
  # (\x:A -> x)[x@0 := (\x:B -> x@1)]
  # (\x:B -> x) -- subtracts one
  def whnf({:app, f, a}) do
    case whnf(f) do
      {:lam, x, _, b} ->
        a1 = shift(1, x, a)
        b1 = subst(x, 0, a1, b)
        whnf(shift(-1, x, b1))

      f1 ->
        {:app, f1, a}
    end
  end

  @spec normalize(expr) :: expr
  def normalize({:var, _} = v), do: v
  def normalize({:const, _} = c), do: c
  def normalize({:pi, x, a, b}), do: {:pi, x, normalize(a), normalize(b)}

  def normalize({:app, f, a}) do
    case normalize(f) do
      {:lam, x, _, b} ->
        a1 = shift(1, x, normalize(a))
        b1 = subst(x, 0, a1, b)
        normalize(shift(-1, x, b1))

      f1 ->
        {:app, f1, normalize(a)}
    end
  end

  # eta reduction =>
  # \x -> f x (x not free in f) => f
  def normalize({:lam, x, a, b}) do
    b1 = normalize(b)

    with {:app, f, {:var, {:v, ^x, 0} = v}} <- b1,
         true <- not free?(v, f) do
      f
    else
      _ -> {:lam, x, normalize(a), b1}
    end
  end

  # as variables are bound in the expression, we build a context of associations, then we check that the variables
  # appear at the same point in the list, whenever that is
  #  \x -> \y -> \z -> z y x
  #  \a -> \b -> \c -> c b a

  #                z,c  -> xl, xr
  #   (x,a) (y,b) (z,c) -> ctx

  @type context :: %{{:v, atom, integer} => expr}
  @spec insert(context, atom, expr) :: context()
  def insert(ctx, x, e) do
    Map.new(ctx, fn
      {{:v, ^x, n}, e1} -> {{:v, x, n + 1}, shift(1, x, e1)}
      {v, e2} -> {v, shift(1, x, e2)}
    end)
    |> Map.put({:v, x, 0}, e)
  end

  @spec match(atom, integer, atom, integer, [{atom, atom}]) :: boolean()
  def match(xl, nl, xr, nr, []), do: xl == xr && nl == nr
  def match(xl, 0, xr, 0, [{xl1, xr1} | _]) when xl == xl1 and xr == xr1, do: true

  def match(xl, nl, xr, nr, [{xl1, xr1} | xs]) do
    match(
      xl,
      if(xl == xl1, do: nl - 1, else: nl),
      xr,
      if(xr == xr1, do: nr - 1, else: nr),
      xs
    )
  end

  @spec eq(expr, expr) :: boolean()
  def eq(e1, e2) do
    go = fn
      {:const, s}, {:const, t}, _, _ ->
        s == t

      {:var, {:v, xl, nl}}, {:var, {:v, xr, nr}}, ctx, _ ->
        match(xl, nl, xr, nr, ctx)

      {:lam, xl, tL, bl}, {:lam, xr, tR, br}, ctx, go ->
        # check types are eq
        if go.(tL, tR, ctx, go) do
          #    add vars to context
          ctx1 = [{xl, xr} | ctx]
          #    check bound sections are eq
          go.(bl, br, ctx1, go)
        else
          false
        end

      {:pi, xl, tL, bl}, {:pi, xr, tR, br}, ctx, go ->
        # same as lambda
        if go.(tL, tR, ctx, go) do
          ctx1 = [{xl, xr} | ctx]
          go.(bl, br, ctx1, go)
        else
          false
        end

      {:app, fl, al}, {:app, fr, ar}, ctx, go ->
        if go.(fl, fr, ctx, go), do: go.(al, ar, ctx, go), else: false

      _, _, _, _ ->
        false
    end

    go.(normalize(e1), normalize(e2), [], go)
  end

  @spec f({:TypeError, [any]} | :ok, {:TypeError, [any]} | :ok) :: {:TypeError, [any]}
  def f(s1, s2) do
    go = fn
      :ok, :ok -> :ok
      {:TypeError, l}, :ok -> {:TypeError, l}
      :ok, {:TypeError, l} -> {:TypeError, l}
      {:TypeError, _}, {:TypeError, l1} -> {:TypeError, l1}
    end

    go.(s1, s2)
  end

  # typeChecks list of expressions, each previous expression provides context for the next one
  # adds a custom type variable, proof, to keep track of the ongoing proof progress, and
  # and the false type, which can never be instantiated, so must be added externally
  @spec typeList([expr]) :: {:ok, [{expr(), context()}]} | {:error, atom, any}
  def typeList(exprl) do
    metactx = %{
      {:v, :result, 0} => {:const, :star}
      # {:v, :False, 0} => {:const, :star},
      # {:v, :f, 0} =>
      # {:pi, :P, {:const, :star}, {:pi, :_, {:var, {:v, :False, 0}}, {:var, {:v, :P, 0}}}}
    }

    go = fn
      [], ctxl, tl, _ ->
        {:ok, List.zip([tl, Enum.reverse(ctxl)])}

      [expr | exspr], [], [], go ->
        case typeWith(metactx, expr) do
          {:ok, t, ctx} -> go.(exspr, [ctx], [t], go)
          {{:TypeError, error}, _, _} -> {:error, :TypeError, error}
        end

      [expr | exspr], [prevctx | xsctx], tl, go ->
        case typeWith(Map.merge(prevctx, metactx), expr) do
          {:ok, t, ctx} -> go.(exspr, [ctx | [prevctx | xsctx]], [t | tl], go)
          {{:TypeError, error}, _, _} -> {:error, :TypeError, error}
        end
    end

    go.(exprl, [], [], go)
  end

  def typeOf(e), do: typeWith(%{}, e)

  @spec typeWith(context, expr) :: {{:TypeError, [any]} | :ok, expr, context}
  def typeWith(
        ctx,
        {:const, c}
      ) do
    case axiom(c) do
      {:ok, s} -> {:ok, {:const, s}, %{}}
      {:error, :TypeError} -> {{:TypeError, [:UntypedBox]}, {:const, c}, ctx}
    end
  end

  # return whatever context we find at the result variable
  def typeWith(
        ctx,
        {:var, {:v, :result, 0}}
      ) do
    {:ok, {:const, :star}, ctx}
  end

  def typeWith(
        ctx,
        {:var, v} = e
      ) do
    case ctx do
      %{^v => t} -> {:ok, t, %{}}
      _ -> {{:TypeError, [:UnboundVariable, e]}, e, ctx}
    end
  end

  def typeWith(
        ctx,
        {:lam, x, tA, b}
      ) do
    {s1, _, rctx1} = typeWith(ctx, tA)
    ctx1 = insert(ctx, x, tA)
    {s2, b1, rctx2} = typeWith(ctx1, b)
    pi = {:pi, x, tA, b1}
    {s3, _, rctx3} = typeWith(ctx, pi)
    {f(f(s1, s2), s3), pi, Map.merge(Map.merge(rctx1, rctx2), rctx3)}
  end

  def typeWith(
        ctx,
        {:pi, x, tA, tB}
      ) do
    {s1, eS, rctx1} = typeWith(ctx, whnf(tA))

    {s2, s} =
      case eS do
        {:const, s} -> {:ok, s}
        _ -> {{:TypeError, [:InvalidInputType]}, :star}
      end

    ctx1 = insert(ctx, x, tA)
    {s3, eT, rctx2} = typeWith(ctx1, whnf(tB))

    {s4, t} =
      case eT do
        {:const, t} -> {:ok, t}
        _ -> {{:TypeError, [:InvalidInputType]}, :star}
      end

    {f(f(f(s1, s2), s3), s4), {:const, rule(s, t)}, Map.merge(rctx1, rctx2)}
  end

  def typeWith(
        ctx,
        {:app, f, a} = e
      ) do
    {s1, e1, rctx1} = typeWith(ctx, f)

    {s2, x, tA, tB} =
      case whnf(e1) do
        {:pi, x, tA, tB} -> {:ok, x, tA, tB}
        _ -> {{:TypeError, [e1, :NotAFunction]}, e, e, e}
      end

    {s3, tA1, rctx2} = typeWith(ctx, a)

    if eq(tA, tA1) do
      a1 = shift(1, x, a)
      tB1 = subst(x, 0, a1, tB)
      {f(f(s1, s2), s3), shift(-1, x, tB1), Map.merge(rctx1, rctx2)}
    else
      nf_A = normalize(tA)
      nf_A1 = normalize(tA1)
      {{:TypeError, [:TypeMismatch, nf_A, :AND, nf_A1]}, e, ctx}
    end
  end
end
