# The little Typer

defmodule CoreTests do
  require CalculusOfInductiveTypes
  use ExUnit.Case
  doctest CalculusOfInductiveTypes

  test "shift doesn't shift bound variables" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> x')
    {:ok, ast} = :parser.parse(tokens)
    e = CalculusOfInductiveTypes.shift(1, :x, ast)
    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :x, 0}}}
  end

  test "shifts unbound, yet aliased var" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> x@1')
    {:ok, ast} = :parser.parse(tokens)
    e = CalculusOfInductiveTypes.shift(1, :x, ast)
    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :x, 2}}}
  end

  test "shifty sequence" do
    e = {:var, {:v, :y, 0}}
    e2 = CalculusOfInductiveTypes.shift(1, :y, e)
    e3 = CalculusOfInductiveTypes.shift(1, :x, e2)
    assert e3 == {:var, {:v, :y, 1}}
  end

  test "subst variable" do
    {:ok, tokens, _} = :lexer.string('f B')
    {:ok, ast} = :parser.parse(tokens)

    e = CalculusOfInductiveTypes.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:app, {:var, {:v, :f, 0}}, {:var, {:v, :y, 0}}}
  end

  test "subst variable in lambda" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> B')
    {:ok, ast} = :parser.parse(tokens)

    e = CalculusOfInductiveTypes.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :y, 0}}}
  end

  test "subst only subst free variables" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> \\(y : *) -> \\(x : *) -> x@1')
    {:ok, ast} = :parser.parse(tokens)

    e = CalculusOfInductiveTypes.subst(:x, 0, {:var, {:v, :y, 0}}, ast)
    assert e == {:lam, :x, {:const, :star}, {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :x, 1}}}}}
  end

  test "subst correct index" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> \\(y : *) -> \\(x : *) -> x@2')
    {:ok, ast} = :parser.parse(tokens)

    # sees two x's on its traversal downward, so incs twice
    e = CalculusOfInductiveTypes.subst(:x, 0, {:var, {:v, :z, 0}}, ast)
    assert e == {:lam, :x, {:const, :star}, {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :z, 0}}}}}
  end

  test "subst never creates bound variable" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> \\(y : *) -> \\(x : *) -> z')
    {:ok, ast} = :parser.parse(tokens)

    e = CalculusOfInductiveTypes.subst(:z, 0, {:var, {:v, :y, 0}}, ast)
    assert e == {:lam, :x, {:const, :star}, {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :y, 1}}}}}
  end

  test "free when v = e" do
    assert CalculusOfInductiveTypes.free?({:v, :x, 0}, {:var, {:v, :x, 0}})
  end

  test "free works in very nested expr" do
    {:ok, tokens, _} = :lexer.string('
    \\(x : *) ->
      \\(y : (\\\/ (a : A) -> B)) ->
        \\(x : *) ->
          x@1 y a x y@1')
    {:ok, ast} = :parser.parse(tokens)
    f? = &CalculusOfInductiveTypes.free?/2
    assert ({false, false, true, true} = {
      f?.({:v, :z, 0}, ast),
      f?.({:v, :x, 0}, ast),
      f?.({:v, :a, 0}, ast),
      f?.({:v, :y, 0}, ast)} # once it sees a y, then only y1 could possibly be free.
      )
  end

  test "whnf application" do
    {:ok, tokens, _} = :lexer.string('(\\(x : * -> *) -> x) (\\(y : *) -> y)')
    {:ok, abnormal} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(y : *) -> y')
    {:ok, whnf} = :parser.parse(tokens)
    assert CalculusOfInductiveTypes.whnf(abnormal) == whnf
  end

  test "whnf works" do
    {:ok, tokens, _} = :lexer.string('(\\(x:A) -> x) (\\(x:B) -> x)')
    {:ok, input} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(x:B) -> x')
    {:ok, whnf} = :parser.parse(tokens)
    assert CalculusOfInductiveTypes.whnf(input) == whnf
  end

  test "whnf respects index" do
    {:ok, tokens, _} = :lexer.string('(\\(x:*) -> \\(y:*) -> \\(x:A) -> x)   z')
    {:ok, bound} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(y:*) -> \\(x:A) -> x')
    {:ok, bound_whnf} = :parser.parse(tokens)


    {:ok, tokens, _} = :lexer.string('(\\(x:*) -> \\(y:*) -> \\(x:A) -> x@1) z')
    {:ok, free} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(y:*) -> \\(x:A) -> z')
    {:ok, free_whnf} = :parser.parse(tokens)

    assert CalculusOfInductiveTypes.whnf(bound) == bound_whnf
    assert CalculusOfInductiveTypes.whnf(free)  == free_whnf

  end

  test "normalize id function on lists" do
    {:ok, tokens, _} = :lexer.string('
    (    \\(List : * -> *)
    ->   \\(map  : forall (a : *) -> forall (b : *) -> (a -> b) -> List a -> List b)
    ->   \\(id   : forall (a : *) -> a -> a)
    ->   \\(a : *) -> map a a (id a)
    )

    -- List
    (\\(a : *) -> forall (x : *) -> (a -> x -> x) -> x -> x)

    -- map
    (   \\(a : *)
    ->  \\(b : *)
    ->  \\(f : a -> b)
    ->  \\(l : forall (x : *) -> (a -> x -> x) -> x -> x)
    ->  \\(x : *)
    ->  \\(Cons : b -> x -> x)
    ->  \\(Nil: x)
    ->  l x (\\(va : a) ->\\(vx : x) -> Cons (f va) vx) Nil
    )

    -- id
    (\\(a : *) -> \\(va : a) -> va)
    ')
    {:ok, abnormal} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(a : *) -> \\(l : \\\/(x : *) -> (a -> x -> x) -> x -> x) -> l')
    {:ok, normal} = :parser.parse(tokens)

    assert CalculusOfInductiveTypes.normalize(abnormal) == normal
  end


  test "normalize edge case from reddit" do
    # this case is here because the github that I looked at for help with this had incorrect behavor on this fn
    {:ok, tokens, _} = :lexer.string('(\\(x : \\\/(x : *) -> *) -> \\(y : *) -> \\(z : *) -> x y) (\\(x : *) -> x)')
    {:ok, abnormal} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(y : *) -> \\(z : *) -> y')
    {:ok, normal} = :parser.parse(tokens)

    assert CalculusOfInductiveTypes.normalize(abnormal) == normal
  end

  test "insert var into context" do
    ctx  = %{{:v, :x, 0} => {:const, :star}, {:v, :y, 0} => {:var, {:v, :x, 0}}}
    ctx1 = CalculusOfInductiveTypes.insert(ctx, :x, {:var, {:v, :y, 0}})

    assert ctx1 == %{{:v, :x, 1} => {:const, :star}, {:v, :y, 0} => {:var, {:v, :x, 1}}, {:v, :x, 0} => {:var, {:v, :y, 0}}}
  end

  test "equivalence of fns" do
    # \x -> \y -> \z -> z y x
  #  \a -> \b -> \c -> c b a
    {:ok, tokens, _} = :lexer.string('\\(x:*) -> \\(y:*) -> \\(z:*) -> z y x')
    {:ok, left} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string('\\(a:*) -> \\(b:*) -> \\(c:*) -> c b a')
    {:ok, right} = :parser.parse(tokens)

    assert CalculusOfInductiveTypes.eq(left, right)
  end

  test "type unbound variable typeerror" do
    ast = {:var, {:v, :x, 0}}
    assert CalculusOfInductiveTypes.typeOf(ast) == {{:TypeError, [:UnboundVariable, {:var, {:v, :x, 0}}]}, {:var, {:v, :x, 0}}, %{}}
  end

  test "type * : box" do
    ast = {:const, :star}
    {:ok, t, _} = CalculusOfInductiveTypes.typeOf(ast)
    assert t  == {:const, :box}
  end

  test "type of lambda :: pi" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> x')
    {:ok, ast} = :parser.parse(tokens)
    {:ok, t, _} = CalculusOfInductiveTypes.typeOf(ast)

    assert t == {:pi, :x, {:const, :star}, {:const, :star}}
  end

  test "type check application of a function" do
    {:ok, tokens, _} = :lexer.string('(\\(x : * -> *) -> x) (\\(y : *) -> y)')
    {:ok, ast} = :parser.parse(tokens)
    {:ok, t, _} = CalculusOfInductiveTypes.typeOf(ast)

    assert t == {:pi, :_, {:const, :star}, {:const, :star}}
  end


  test "type id on lists" do
    {:ok, tokens, _} = :lexer.string('
    (    \\(List : * -> *)
    ->   \\(map  : forall (a : *) -> forall (b : *) -> (a -> b) -> List a -> List b)
    ->   \\(id   : forall (a : *) -> a -> a)
    ->   \\(a : *) -> map a a (id a)
    )

    -- List
    (\\(a : *) -> forall (x : *) -> (a -> x -> x) -> x -> x)

    -- map
    (   \\(a : *)
    ->  \\(b : *)
    ->  \\(f : a -> b)
    ->  \\(l : forall (x : *) -> (a -> x -> x) -> x -> x)
    ->  \\(x : *)
    ->  \\(Cons : b -> x -> x)
    ->  \\(Nil: x)
    ->  l x (\\(va : a) ->\\(vx : x) -> Cons (f va) vx) Nil
    )

    -- id
    (\\(a : *) -> \\(va : a) -> va)
    ')
    {:ok, ast} = :parser.parse(tokens)
    {:ok, t, ctx} = CalculusOfInductiveTypes.typeOf(ast)

    {:ok, tokens, _} = :lexer.string('\\\/(a : *) -> (\\\/(x : *) -> (a -> x -> x) -> x -> x) -> \\\/(x : *) -> (a -> x -> x) -> x -> x')
    {:ok, ast} = :parser.parse(tokens)

    assert CalculusOfInductiveTypes.eq(t,ast)
  end

  test "type of P -> Q -> P for all (P, Q) : *" do
    {:ok, tokens, _} = :lexer.string('\\(C : *) -> \\(P : *) -> \\(Q : *) ->
      let (th1 : (forall(P : *) -> forall(Q : *) -> P -> Q -> P)) =
      \\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\\(hq : Q) -> hp in C')
    {:ok, ast} = :parser.parse(tokens)

    {:ok, _, _} = CalculusOfInductiveTypes.typeOf(ast)
  end


  test "modus ponens" do
    {:ok, tokens, _} = :lexer.string('\\(A : *) -> \\(B : *)
    ->
      \\(Mp : A -> (A -> B) -> B) -> \\(h1 : A) -> \\(h2 : A -> B) -> h2 h1')

    {:ok, ast} = :parser.parse(tokens)

    {:ok, t, _} = CalculusOfInductiveTypes.typeOf(ast)

    assert t == {:pi, :A, {:const, :star}, {:pi, :B, {:const, :star}, {:pi, :Mp, {:pi, :_, {:var, {:v, :A, 0}}, {:pi, :_, {:pi, :_, {:var, {:v, :A, 0}}, {:var, {:v, :B, 0}}}, {:var, {:v, :B, 0}}}}, {:pi, :h1, {:var, {:v, :A, 0}}, {:pi, :h2, {:pi, :_, {:var, {:v, :A, 0}}, {:var, {:v, :B, 0}}}, {:var, {:v, :B, 0}}}}}}}
  end

end
