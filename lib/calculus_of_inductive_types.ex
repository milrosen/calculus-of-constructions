defmodule CalculusOfInductiveTypes do
  @type const :: :star | :box
  @type error :: :TypeError

  @spec axiom(const) :: {:ok, const} | {:error, :TypeError}
  def axiom(:star), do: {:ok, :box}
  def axiom(:box),  do: {:error, :TypeError}

  @spec rule(const, const) :: const

  # * ⤳ *, or terms can depend on terms. This rule is equivalent to the simply typed lambda calculus
  def rule(:star, :star), do: :star

  # □ ⤳ *, or terms can depend on types. This is eqivalent to the F_2, where types and terms are fundamentally different
  def rule(:box, :star),  do: :star

  # □ ⤳ □, or types can depend on types. This enables Meta types, like Tree<T> and so on
  def rule(:box, :box),   do: :box

  # * ⤳ □, or types can depend on terms. This is sometimes called "dependent typing" and makes this the CoC
  def rule(:star, :box),  do: :box


  @type expr ::
    {:const, const} |
    {:var, {:v, atom, integer}} |
    {:lam, atom, expr, expr} |
    {:pi,  atom, expr, expr} |
    {:app, expr, expr}

  # adds n to each free x@n in e.
  @spec shift(integer, atom, expr) :: expr
  def shift(d, x0, e0) do
    #the @n indexes how nested the binding of X is. Following diagram from Morte.hs library, where the code is based.big
    # >                           +-refers to-+
    # >                           |           |
    # >                           v           |
    # > \(x : *) -> \(y : *) -> \(x : *) -> x@0
    # >
    # >   +-------------refers to-------------+
    # >   |                                   |
    # >   v                                   |
    # > \(x : *) -> \(y : *) -> \(x : *) -> x@1

    # first cases are counting how many bound x's there are, then the var case increments by initial value + how many we counted
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

  # Rule for application
  #  Γ⊢ f : (Πx : A. B)       Γ⊢ a : A
  #          Γ⊢ f a : B[x := a]
  #
  # read, with some f : A->B (where B could depend on x), and a : A,
  # f a is of type B[x := a]

  # note that the lambda and Pi rules are identical. Isn't that cool!!
  # this is because we are in the CoC, and the application rule is the same for types and functions,
  # where the type of a function is expressed as a dependent product, reguardless of what its abstracting

  # substitue(n x C B) -> B[x@n := C]
  @spec subst(atom, integer, expr, expr) :: expr
  def subst(x, n, e1,
  {:var, {:v, x1, n1}}), do: (if x1 == x && n1 == n, do: e1, else: {:var, {:v, x1, n1}})

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

end
