defmodule CalculusOfInductiveTypes do
  @type const :: :star | :box
  @type error :: :TypeError

  @spec axiom(const) :: {:ok, const} | {:error, :TypeError}
  def axiom(:star), do: {:ok, :box}
  def axiom(:box),  do: {:error, :TypeError}

  @spec rule(const, const) :: const

  # * ⤳ *, or terms can depend on terms. This rule is equivalent to the simply typed lambda calculus
  def rule(:star, :star), do: :star

  # □ ⤳ *, or terms can depend on types. This is eqivalent to the F2, where types and terms are fundamentally different
  def rule(:box, :star),  do: :star

  # □ ⤳ □, or types can depend on types. This enables Meta types, like Tree<T> and so on
  def rule(:box, :box),   do: :box

  # * ⤳ □, or types can depend on terms. This is sometimes called "dependent typing" and makes this the CoC
  def rule(:star, :box),  do: :box


  @type expr ::
    {:const, const} |
    {:var, {:v, atom, integer}} |
    {:lam, atom, expr(), expr()} |
    {:pi,  atom, expr(), expr()} |
    {:app, expr, expr()}

  # adds n to each free x@n in e.
  @spec shift(integer, atom, expr) :: expr
  def shift(d, x0, e0) do
    #the @n indexes how nested the binding of X is. Following diagram from Morte.hs library
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
      {:lam, x, a, b},    c, go -> {:lam, x, go.(a, c, go), go.(b, (if x == x0, do: c+1, else: c), go)}
      {:pi,  x, a, b},    c, go -> {:pi,  x, go.(a, c, go), go.(b, (if x == x0, do: c+1, else: c), go)}
      {:app, f, a},       c, go -> {:app, go.(f, c, go), go.(a, c, go)}
      {:var, {:v, x, n}}, c, _  -> {:var, {:v, x, (if x == x0 && n >= c, do: d+n, else: n)}}
      {:const, s},        _, _  -> {:const, s}
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
  def subst(x, n, e1,
  {:var, {:v, x1, n1}}  = v), do: (if x1 == x && n1 == n, do: e1, else: v)

  def subst(_, _, _,
  {:const, const}), do: {:const, const}

  def subst(x, n, e1,
  {:app, f, a}), do: {:app, subst(x, n, e1, f), subst(x, n, e1, a)}

  def subst(x, n, e1,
  {:lam, y, a, b}) do
    n1 = if x == y, do: n + 1, else: n
    b1 = subst(x, n1, shift(1, y, e1), b)

    {:lam, y, subst(x, n, e1, a), b1}
  end

  def subst(x, n, e1,
  {:pi, y, a, b}) do
    n1 = if x == y, do: n + 1, else: n
    b1 = subst(x, n1, shift(1, y, e1), b)

    {:pi, y, subst(x, n, e1, a), b1}
  end

  # V free in e?
  @spec free?({:v, atom, integer}, expr) :: boolean
  def free?({:v, _, _} = v,
  {:var, v1}), do: v == v1

  def free?({:v, _, _},
  {:const, _}), do: false

  def free?({:v, _, _} = v,
  {:app, f, a}), do: free?(v, f) || free?(v, a)

  def free?({:v, x, n} = v,
  {:lam, x1, a, b}) do
    free?(v, a) || if x == x1, do: free?({:v, x, n+1}, b), else: free?(v, b)
  end

  def free?({:v, x, n} = v,
  {:pi, x1, a, b}) do
    free?(v, a) || if x == x1, do: free?({:v, x, n+1}, b), else: free?(v, b)
  end


  # weak-head-normal form. Basically, no more outside applications to make \x -> 1 + 2 is, but (\x -> 1) 2 isn't
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
      f1 -> {:app, f1, a}
    end
  end

  @spec normalize(expr) :: expr
  def normalize({:var, _} = v), do: v
  def normalize({:const, _} = c), do: c
  def normalize({:pi, x, a, b}),  do: {:pi, x, normalize(a), normalize(b)}

  def normalize({:app, f, a}) do
    case normalize(f) do
      {:lam, x, _, b} ->
        a1 = shift(1, x, normalize(a))
        b1 = subst(x, 0, a1, b)
        normalize(shift(-1, x, b1))
      f1 -> {:app, f1, normalize(a)}
    end
  end

  # ETA =>
  # \x -> f x (x not free in f) => f
  # question to consider for later, does change the behavior or is it just an optimization
  def normalize({:lam, x, a, b}) do
    b1 = normalize(b)
    with {:app, f, {:var, {:v, ^x, 0}=v}} <- b1,
         true <- not free?(v, f) do f
    else
      _ -> {:lam, x, normalize(a), b1}
    end
  end


end
