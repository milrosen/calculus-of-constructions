defmodule CoreTests do
  require Core
  require PrettyPrint
  use ExUnit.Case
  doctest Core

  defp single_expr(str) do
    {:ok, tokens, _} = :lexer.string(str |> to_charlist())
    {:ok, [ast]} = :parser.parse(tokens)
    curried = Desugar.curry(ast)
    {:ok, deltad} = Desugar.delta(%{}, curried)
    deltad
  end

  test "shift doesn't shift bound variables" do
    ast = single_expr("\\(x:*) -> x")

    e = Core.shift(1, :x, ast)
    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :x, 0}}}
  end

  test "shifts unbound, yet aliased var" do
    ast = single_expr("\\(x : *) -> x@1")

    e = Core.shift(1, :x, ast)
    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :x, 2}}}
  end

  test "shifty sequence" do
    e = {:var, {:v, :y, 0}}
    e2 = Core.shift(1, :y, e)
    e3 = Core.shift(1, :x, e2)
    assert e3 == {:var, {:v, :y, 1}}
  end

  test "subst variable" do
    ast = single_expr("f B")

    e = Core.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:app, {:var, {:v, :f, 0}}, {:var, {:v, :y, 0}}}
  end

  test "subst variable in lambda" do
    ast = single_expr("\\(x:*) -> B")

    e = Core.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :y, 0}}}
  end

  test "subst only subst free variables" do
    ast = single_expr(~c"\\(x : *) -> \\(y : *) -> \\(x : *) -> x@1")

    e = Core.subst(:x, 0, {:var, {:v, :y, 0}}, ast)

    assert e ==
             {:lam, :x, {:const, :star},
              {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :x, 1}}}}}
  end

  test "subst correct index" do
    ast = single_expr("\\(x : *) -> \\(y : *) -> \\(x : *) -> x@2")

    # sees two x's on its traversal downward, so incs twice
    e = Core.subst(:x, 0, {:var, {:v, :z, 0}}, ast)

    assert e ==
             {:lam, :x, {:const, :star},
              {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :z, 0}}}}}
  end

  test "subst never creates bound variable" do
    ast = single_expr("\\(x : *) -> \\(y : *) -> \\(x : *) -> z")

    e = Core.subst(:z, 0, {:var, {:v, :y, 0}}, ast)

    assert e ==
             {:lam, :x, {:const, :star},
              {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :y, 1}}}}}
  end

  test "free when v = e" do
    assert Core.free?({:v, :x, 0}, {:var, {:v, :x, 0}})
  end

  test "free works in very nested expr" do
    ast = single_expr("
    \\(x : *) ->
      \\(y : (\\\/ (a : A) -> B)) ->
        \\(x : *) ->
          x@1 y a x y@1")
    f? = &Core.free?/2

    assert {false, false, true, true} = {
             f?.({:v, :z, 0}, ast),
             f?.({:v, :x, 0}, ast),
             f?.({:v, :a, 0}, ast),
             # once it sees a y, then only y1 could possibly be free.
             f?.({:v, :y, 0}, ast)
           }
  end

  test "whnf application" do
    abnormal = single_expr("(\\(x : * -> *) -> x) (\\(y : *) -> y)")

    whnf = single_expr("\\(y : *) -> y")
    assert Core.whnf(abnormal) == whnf
  end

  test "whnf works" do
    input = single_expr("(\\(x:A) -> x) (\\(x:B) -> x)")

    whnf = single_expr("\\(x:B) -> x")
    assert Core.whnf(input) == whnf
  end

  test "whnf respects index" do
    bound = single_expr("(\\(x:*) -> \\(y:*) -> \\(x:A) -> x)   z")

    bound_whnf = single_expr("\\(y:*) -> \\(x:A) -> x")

    free = single_expr("(\\(x:*) -> \\(y:*) -> \\(x:A) -> x@1) z")

    free_whnf = single_expr("\\(y:*) -> \\(x:A) -> z")

    assert Core.whnf(bound) == bound_whnf
    assert Core.whnf(free) == free_whnf
  end

  test "normalize id function on lists" do
    abnormal = single_expr("
    (    \\(List : * -> *)
    ->   \\(map  : {a : *} {b : *} (a -> b) -> List a -> List b)
    ->   \\(id   : {a : *} a -> a)
    ->   \\(a : *) -> map a a (id a)
    )

    -- List
    (\\(a : *) -> {x : *} (a -> x -> x) -> x -> x)

    -- map
    (   \\(a : *)
    ->  \\(b : *)
    ->  \\(f : a -> b)
    ->  \\(l : {x : *} (a -> x -> x) -> x -> x)
    ->  \\(x : *)
    ->  \\(Cons : b -> x -> x)
    ->  \\(Nil: x)
    ->  l x (\\(va : a) ->\\(vx : x) -> Cons (f va) vx) Nil
    )

    -- id
    (\\(a : *) -> \\(va : a) -> va)
    ")

    normal = single_expr("\\(a : *) -> \\(l : \\\/(x : *) -> (a -> x -> x) -> x -> x) -> l")

    assert Core.normalize(abnormal) == normal
  end

  test "normalize edge case from reddit" do
    # this case is here because the github that I looked at for help with this had incorrect behavor on this fn
    abnormal =
      single_expr("(\\(x : \\\/(x : *) -> *) -> \\(y : *) -> \\(z : *) -> x y) (\\(x : *) -> x)")

    normal = single_expr("\\(y : *) -> \\(z : *) -> y")

    assert Core.normalize(abnormal) == normal
  end

  test "insert var into context" do
    ctx = %{{:v, :x, 0} => {:const, :star}, {:v, :y, 0} => {:var, {:v, :x, 0}}}
    ctx1 = Core.insert(ctx, :x, {:var, {:v, :y, 0}})

    assert ctx1 == %{
             {:v, :x, 1} => {:const, :star},
             {:v, :y, 0} => {:var, {:v, :x, 1}},
             {:v, :x, 0} => {:var, {:v, :y, 0}}
           }
  end

  test "equivalence of fns" do
    # \x -> \y -> \z -> z y x
    #  \a -> \b -> \c -> c b a
    left = single_expr("\\(x:*) -> \\(y:*) -> \\(z:*) -> z y x")

    right = single_expr("\\(a:*) -> \\(b:*) -> \\(c:*) -> c b a")

    assert Core.eq(left, right)
  end

  test "type unbound variable typeerror" do
    ast = {:var, {:v, :x, 0}}

    assert match?(
             {:UnboundVariableError,
              [
                {:var, {:v, :x, 0}},
                _
              ]},
             Core.typeOf(ast)
           )
  end

  test "type * : box" do
    ast = {:const, :star}
    {:ok, t} = Core.typeOf(ast)
    assert t == {:const, :box}
  end

  test "type of lambda :: pi" do
    ast = single_expr("\\(x : *) -> x")
    {:ok, t} = Core.typeOf(ast)

    assert t == {:pi, :x, {:const, :star}, {:const, :star}}
  end

  test "type check application of a function" do
    ast = single_expr("(\\(x : * -> *) -> x) (\\(y : *) -> y)")
    {:ok, t} = Core.typeOf(ast)

    assert t == {:pi, :_, {:const, :star}, {:const, :star}}
  end

  test "type id on lists" do
    ast = single_expr("
    (    \\(List : * -> *)
    ->   \\(map  : {a : *} {b : *} (a -> b) -> List a -> List b)
    ->   \\(id   : {a : *} a -> a)
    ->   \\(a : *) -> map a a (id a)
    )

    -- List
    (\\(a : *) -> {x : *} (a -> x -> x) -> x -> x)

    -- map
    (   \\(a : *)
    ->  \\(b : *)
    ->  \\(f : a -> b)
    ->  \\(l : {x : *} (a -> x -> x) -> x -> x)
    ->  \\(x : *)
    ->  \\(Cons : b -> x -> x)
    ->  \\(Nil: x)
    ->  l x (\\(va : a) ->\\(vx : x) -> Cons (f va) vx) Nil
    )

    -- id
    (\\(a : *) -> \\(va : a) -> va)
    ")
    {:ok, t} = Core.typeOf(ast)

    ast =
      single_expr(
        "\\\/(a : *) -> (\\\/(x : *) -> (a -> x -> x) -> x -> x) -> \\\/(x : *) -> (a -> x -> x) -> x -> x"
      )

    assert Core.eq(t, ast)
  end

  test "And" do
    {:ok, [_, _, {:eval, _term, type}]} = CalculusOfConstructions.check("
    #def and := fun a : *, b : *. {c : *} (a -> b -> c) -> c
    #def K := fun a : *, b : *, x : a, y : b. x
    #eval fun a : *, b : *. fun x : and a b . x a (K a b)")

    assert PrettyPrint.printExpr(type) ==
             "Π(a : *) → Π(b : *) → Π(x : Π(c : *) → (a → b → c) → c) → a"
  end

  test "Sum types" do
    # What is a sum type, really?
    # A thing that you can match on, or, more accurately, a thing that you HAVE to match on
    # If I say T is a string, int or bool, then any fn accepting a T has to be ok recieving a string, int or bool
    ast = single_expr("( (
    \\(T : * -> * -> * -> *)

         -- The value constructors
     ->  \\(A : {a : *} {b : *} {c : *}           T a b c)
     ->  \\(B : {a : *} {b : *} {c : *} a ->      T a b c)
     ->  \\(C : {a : *} {b : *} {c : *} b -> c -> T a b c)

         -- Pattern match on T
     ->  \\(  matchT
         :   {a : *} {b : *} {c : *}
             T a b c
         ->  {r : *}
             r              -- `A` branch of the pattern match
         ->  (a -> r)       -- `B` branch of the pattern match
         ->  (b -> c -> r)  -- `C` branch of the pattern match
         ->  r
         )
     -> \\(String : *) -> \\(h : String) -> \\(Bool : *) -> \\(t : Bool)
     -> matchT String String Bool (C String String Bool h t) String
        h
        (\\(s : String) -> s)
        (\\(a : String) -> \\(tf : Bool) -> a)
     )

     -- A value of type `T a b c` is just a preformed pattern match
     (   \\(a : *) -> \\(b : *) -> \\(c : *)
     ->  forall (r : *)
     ,    r              -- A branch of the pattern match
     ->  (a -> r)       -- B branch of the pattern match
     ->  (b -> c -> r)  -- C branch of the pattern match
     ->  r
     )

     -- Constructor for A
     (   \\(a : *)
     ->  \\(b : *)
     ->  \\(c : *)
     ->  \\(r : *)
     ->  \\(A : r)
     ->  \\(B : a -> r)
     ->  \\(C : b -> c -> r)
     ->  A
     )

     -- Constructor for B
     (   \\(a : *)
     ->  \\(b : *)
     ->  \\(c : *)
     ->  \\(va : a)
     ->  \\(r : *)
     ->  \\(A : r)
     ->  \\(B : a -> r)
     ->  \\(C : b -> c -> r)
     ->  B va
     )

     -- Constructor for C
     (   \\(a : *)
     ->  \\(b : *)
     ->  \\(c : *)
     ->  \\(vb : b)
     ->  \\(vc : c)
     ->  \\(r : *)
     ->  \\(A : r)
     ->  \\(B : a -> r)
     ->  \\(C : b -> c -> r)
     ->  C vb vc
     )

     -- matchT is just the identity function
     (   \\(a : *)
     ->  \\(b : *)
     ->  \\(c : *)
     ->  \\(t : forall (r : *), r -> (a -> r) -> (b -> c -> r) -> r)
     ->  t
     ))")
    {:ok, t} = Core.typeOf(ast)
    _norm = Core.normalize(ast)

    assert t ==
             {:pi, :String, {:const, :star},
              {:pi, :h, {:var, {:v, :String, 0}},
               {:pi, :Bool, {:const, :star},
                {:pi, :t, {:var, {:v, :Bool, 0}}, {:var, {:v, :String, 0}}}}}}
  end

  test "application order" do
    ast1 = single_expr("\\(p : *) -> \\(q: *) ->
                                      ((\\(x:*) -> \\(y:*) -> y) p q)")

    ast2 = single_expr("\\(p : *) -> \\(q: *) ->
                                      ((\\(x:*) -> (\\(y:*) -> y) q) p)")

    assert Core.typeOf(ast1) == Core.typeOf(ast2)
    assert Core.normalize(ast1) == Core.normalize(ast2)
  end

  test "natural number induction type" do
    ast1 = single_expr("fun target : Nat, mot : Nat -> *, base : (mot zero),
        step : ({nm1 : Nat} (mot nm1) -> (mot (succ nm1))) . indNat target mot base step")

    ast2 = single_expr("indNat")

    assert Core.typeOf(ast1) == Core.typeOf(ast2)
  end

  test "normalize types" do
    ast1 = single_expr("
    fun a : Nat, b : Nat .
      (fun x : Nat.x) a")

    ast2 = single_expr("
    fun a : Nat, b : Nat . a")

    assert Core.typeOf(ast1) == Core.typeOf(ast2)
  end

  test "plus" do
    {:ok, t1} = single_expr("fun a : Nat, b : Nat .
        indNat a (fun x : Nat. Nat) b
          (fun _ : Nat, pm1 : Nat. succ pm1)") |> Core.typeOf()

    {:ok, t2} = single_expr("fun a : Nat, b : Nat. succ a") |> Core.typeOf()

    assert Core.eq(t1, t2)

    {:ok, [_, {:eval, four, _}]} =
      CalculusOfConstructions.check("
      #def (+) := fun a : Nat, b : Nat .
        indNat a (fun x : Nat. Nat) b
          (fun _ : Nat, pm1 : Nat . succ pm1)
      #eval (+) (succ (succ zero)) (succ (succ zero))")

    assert four == single_expr("succ (succ (succ (succ zero)))")
  end
end
