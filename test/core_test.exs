defmodule CoreTests do
  require Core
  require PrettyPrint
  use ExUnit.Case
  doctest Core

  test "shift doesn't shift bound variables" do
    {:ok, tokens, _} = :lexer.string(~c"\\(x : *) -> x")
    {:ok, ast} = :parser.parse(tokens)
    e = Core.shift(1, :x, ast)
    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :x, 0}}}
  end

  test "shifts unbound, yet aliased var" do
    {:ok, tokens, _} = :lexer.string(~c"\\(x : *) -> x@1")
    {:ok, ast} = :parser.parse(tokens)
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
    {:ok, tokens, _} = :lexer.string(~c"f B")
    {:ok, ast} = :parser.parse(tokens)

    e = Core.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:app, {:var, {:v, :f, 0}}, {:var, {:v, :y, 0}}}
  end

  test "subst variable in lambda" do
    {:ok, tokens, _} = :lexer.string(~c"\\(x : *) -> B")
    {:ok, ast} = :parser.parse(tokens)

    e = Core.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :y, 0}}}
  end

  test "subst only subst free variables" do
    {:ok, tokens, _} = :lexer.string(~c"\\(x : *) -> \\(y : *) -> \\(x : *) -> x@1")
    {:ok, ast} = :parser.parse(tokens)

    e = Core.subst(:x, 0, {:var, {:v, :y, 0}}, ast)

    assert e ==
             {:lam, :x, {:const, :star},
              {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :x, 1}}}}}
  end

  test "subst correct index" do
    {:ok, tokens, _} = :lexer.string(~c"\\(x : *) -> \\(y : *) -> \\(x : *) -> x@2")
    {:ok, ast} = :parser.parse(tokens)

    # sees two x's on its traversal downward, so incs twice
    e = Core.subst(:x, 0, {:var, {:v, :z, 0}}, ast)

    assert e ==
             {:lam, :x, {:const, :star},
              {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :z, 0}}}}}
  end

  test "subst never creates bound variable" do
    {:ok, tokens, _} = :lexer.string(~c"\\(x : *) -> \\(y : *) -> \\(x : *) -> z")
    {:ok, ast} = :parser.parse(tokens)

    e = Core.subst(:z, 0, {:var, {:v, :y, 0}}, ast)

    assert e ==
             {:lam, :x, {:const, :star},
              {:lam, :y, {:const, :star}, {:lam, :x, {:const, :star}, {:var, {:v, :y, 1}}}}}
  end

  test "free when v = e" do
    assert Core.free?({:v, :x, 0}, {:var, {:v, :x, 0}})
  end

  test "free works in very nested expr" do
    {:ok, tokens, _} = :lexer.string(~c"
    \\(x : *) ->
      \\(y : (\\\/ (a : A) -> B)) ->
        \\(x : *) ->
          x@1 y a x y@1")
    {:ok, ast} = :parser.parse(tokens)
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
    {:ok, tokens, _} = :lexer.string(~c"(\\(x : * -> *) -> x) (\\(y : *) -> y)")
    {:ok, abnormal} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"\\(y : *) -> y")
    {:ok, whnf} = :parser.parse(tokens)
    assert Core.whnf(abnormal) == whnf
  end

  test "whnf works" do
    {:ok, tokens, _} = :lexer.string(~c"(\\(x:A) -> x) (\\(x:B) -> x)")
    {:ok, input} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"\\(x:B) -> x")
    {:ok, whnf} = :parser.parse(tokens)
    assert Core.whnf(input) == whnf
  end

  test "whnf respects index" do
    {:ok, tokens, _} = :lexer.string(~c"(\\(x:*) -> \\(y:*) -> \\(x:A) -> x)   z")
    {:ok, bound} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"\\(y:*) -> \\(x:A) -> x")
    {:ok, bound_whnf} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"(\\(x:*) -> \\(y:*) -> \\(x:A) -> x@1) z")
    {:ok, free} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"\\(y:*) -> \\(x:A) -> z")
    {:ok, free_whnf} = :parser.parse(tokens)

    assert Core.whnf(bound) == bound_whnf
    assert Core.whnf(free) == free_whnf
  end

  test "normalize id function on lists" do
    {:ok, tokens, _} = :lexer.string(~c"
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
    ")
    {:ok, abnormal} = :parser.parse(tokens)

    {:ok, tokens, _} =
      :lexer.string(~c"\\(a : *) -> \\(l : \\\/(x : *) -> (a -> x -> x) -> x -> x) -> l")

    {:ok, normal} = :parser.parse(tokens)

    assert Core.normalize(abnormal) == normal
  end

  test "normalize edge case from reddit" do
    # this case is here because the github that I looked at for help with this had incorrect behavor on this fn
    {:ok, tokens, _} =
      :lexer.string(
        ~c"(\\(x : \\\/(x : *) -> *) -> \\(y : *) -> \\(z : *) -> x y) (\\(x : *) -> x)"
      )

    {:ok, abnormal} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"\\(y : *) -> \\(z : *) -> y")
    {:ok, normal} = :parser.parse(tokens)

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
    {:ok, tokens, _} = :lexer.string(~c"\\(x:*) -> \\(y:*) -> \\(z:*) -> z y x")
    {:ok, left} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"\\(a:*) -> \\(b:*) -> \\(c:*) -> c b a")
    {:ok, right} = :parser.parse(tokens)

    assert Core.eq(left, right)
  end

  test "type unbound variable typeerror" do
    ast = {:var, {:v, :x, 0}}

    assert Core.typeOf(ast) ==
             {{:TypeError, [:UnboundVariable, {:var, {:v, :x, 0}}]}, {:var, {:v, :x, 0}}, %{}}
  end

  test "type * : box" do
    ast = {:const, :star}
    {:ok, t, _} = Core.typeOf(ast)
    assert t == {:const, :box}
  end

  test "type of lambda :: pi" do
    {:ok, tokens, _} = :lexer.string(~c"\\(x : *) -> x")
    {:ok, ast} = :parser.parse(tokens)
    {:ok, t, _} = Core.typeOf(ast)

    assert t == {:pi, :x, {:const, :star}, {:const, :star}}
  end

  test "type check application of a function" do
    {:ok, tokens, _} = :lexer.string(~c"(\\(x : * -> *) -> x) (\\(y : *) -> y)")
    {:ok, ast} = :parser.parse(tokens)
    {:ok, t, _} = Core.typeOf(ast)

    assert t == {:pi, :_, {:const, :star}, {:const, :star}}
  end

  test "type id on lists" do
    {:ok, tokens, _} = :lexer.string(~c"
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
    ")
    {:ok, ast} = :parser.parse(tokens)
    {:ok, t, ctx} = Core.typeOf(ast)

    {:ok, tokens, _} =
      :lexer.string(
        ~c"\\\/(a : *) -> (\\\/(x : *) -> (a -> x -> x) -> x -> x) -> \\\/(x : *) -> (a -> x -> x) -> x -> x"
      )

    {:ok, ast} = :parser.parse(tokens)

    assert Core.eq(t, ast)
  end

  test "And elimination, Modus Ponens" do
    {:ok, tokens, _} = :lexer.string(~c"\\(result : *) ->
      (  \\(th1 : forall(P : *) -> forall(Q : *) -> P -> Q -> P)
      -> \\(th2 : forall(P : *) -> forall(Q : *) -> P -> (P -> Q) -> Q )
      -> result)
      (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp)
      (\\(P : *) -> \\(Q : *) -> \\(h1 : P) -> \\(h2 : P -> Q) -> h2 h1)
      ")
    {:ok, ast} = :parser.parse(tokens)

    {:ok, t, _} = Core.typeOf(ast)
    assert t == {:pi, :result, {:const, :star}, {:const, :star}}
  end

  test "Result Variable" do
    {:ok, tokens, _} = :lexer.string(~c"
      (  \\(th1 : forall(P : *) -> forall(Q : *) -> P -> Q -> P)
      -> \\(th2 : forall(P : *) -> forall(Q : *) -> P -> (P -> Q) -> Q )
      -> result)
      (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp)
      (\\(P : *) -> \\(Q : *) -> \\(h1 : P) -> \\(h2 : P -> Q) -> h2 h1)
      ")
    {:ok, ast} = :parser.parse(tokens)

    {:ok, _, ctx} = Core.typeOf(ast)

    assert ctx == %{
             {:v, :th1, 0} =>
               {:pi, :P, {:const, :star},
                {:pi, :Q, {:const, :star},
                 {:pi, :_, {:var, {:v, :P, 0}},
                  {:pi, :_, {:var, {:v, :Q, 0}}, {:var, {:v, :P, 0}}}}}},
             {:v, :th2, 0} =>
               {:pi, :P, {:const, :star},
                {:pi, :Q, {:const, :star},
                 {:pi, :_, {:var, {:v, :P, 0}},
                  {:pi, :_, {:pi, :_, {:var, {:v, :P, 0}}, {:var, {:v, :Q, 0}}},
                   {:var, {:v, :Q, 0}}}}}}
           }
  end

  test "TypeList" do
    {:ok, tokens1, _} = :lexer.string(~c"
      (\\(th1 : forall(P : *) -> forall(Q : *) -> P -> Q -> P) -> result)
      (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hp)")
    {:ok, tokens2, _} = :lexer.string(~c"
      (\\(th2 : forall(P : *) -> forall(Q : *) -> P -> (P -> Q) -> Q) -> result)
      (\\(P : *) -> \\(Q : *) -> \\(h1 : P) -> \\(h2 : P -> Q) -> h2 h1)")
    {:ok, ast1} = :parser.parse(tokens1)
    {:ok, ast2} = :parser.parse(tokens2)

    list = Core.typeList([ast1, ast2])
    assert {:ok, [{{:const, :star}, _}, {{:const, :star}, _}]} = list
  end

  test "TypeList Errors" do
    {:ok, tokens, _} = :lexer.string(~c"
      (\\(th1 : forall(P : *) -> forall(Q : *) -> P -> Q -> P) -> result)
      (\\(P : *) -> \\(Q : *) -> \\(hp : P) -> \\(hq : Q) -> hq)")

    {:ok, ast} = :parser.parse(tokens)

    list = Core.typeList([ast])
    assert {:error, :TypeError, error} = list
    IO.puts(PrettyPrint.printError({:error, :TypeError, error}))
  end

  test "what is a value constructor" do
    {:ok, tokens, _} = :lexer.string(~c"( (   \\(T : * -> * -> * -> *)

         -- The value constructors
     ->  \\(A : forall (a : *) -> forall (b : *) -> forall (c : *)           -> T a b c)
     ->  \\(B : forall (a : *) -> forall (b : *) -> forall (c : *) -> a      -> T a b c)
     ->  \\(C : forall (a : *) -> forall (b : *) -> forall (c : *) -> b -> c -> T a b c)

         -- Pattern match on T
     ->  \\(  matchT
         :   forall (a : *) -> forall (b : *) -> forall (c : *)
         ->  T a b c
         ->  forall (r : *)
         ->  r              -- `A` branch of the pattern match
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
     ->  r              -- A branch of the pattern match
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
     ->  \\(t : forall (r : *) -> r -> (a -> r) -> (b -> c -> r) -> r)
     ->  t
     ))")
    {:ok, ast} = :parser.parse(tokens)
    {:ok, t, _} = Core.typeOf(ast)
    norm = Core.normalize(ast)

    assert t ==
             {:pi, :String, {:const, :star},
              {:pi, :h, {:var, {:v, :String, 0}},
               {:pi, :Bool, {:const, :star},
                {:pi, :t, {:var, {:v, :Bool, 0}}, {:var, {:v, :String, 0}}}}}}
  end

  test "application order" do
    {:ok, tokens, _} = :lexer.string(~c"\\(p : *) -> \\(q: *) ->
                                      ((\\(x:*) -> \\(y:*) -> y) p q)")
    {:ok, ast1} = :parser.parse(tokens)

    {:ok, tokens, _} = :lexer.string(~c"\\(p : *) -> \\(q: *) ->
                                      ((\\(x:*) -> (\\(y:*) -> y) q) p)")
    {:ok, ast2} = :parser.parse(tokens)

    assert Core.typeOf(ast1) == Core.typeOf(ast2)
    assert Core.normalize(ast1) == Core.normalize(ast2)
  end

  test "zero dne 1" do
    {:ok, tokens, _} = :lexer.string(~c"\\(truth : *) ->
      ((\\(Nat : *) ->
        \\(Succ : Nat -> Nat) ->
        \\(Zero : Nat) ->
        \\(foldNat : Nat -> forall(r : *) -> (r -> r) -> r -> r)
        -> foldNat)
      (
        -- Nat
        forall(nat : *)
        -> (nat -> nat) -- Succ
        -> nat          -- Zero
        -> nat
      )
      (
        \\(pred : forall(nat : *) -> (nat -> nat) -> nat -> nat) -- forall nats, sucessor of a nat is a nat
        -> \\(nat : *)
        -> \\(Succ : nat -> nat)
        -> \\(Zero : nat)
        -> Succ (pred nat Succ Zero)
      )
      (
        \\(nat : *)
        -> \\(Succ : nat -> nat)
        -> \\(Zero : nat)
        -> Zero
      )
      (
        \\(n : forall(nat : *) -> (nat -> nat) -> nat -> nat)
        -> n
      )
      )")

    {:ok, ast} = :parser.parse(tokens)

    {:ok, e, _} = Core.typeOf(ast)

    IO.puts(PrettyPrint.printExpr(e))
  end

  test "errors" do
    {:ok, tokens, _} = :lexer.string(~c"Just Text")
    {:ok, ast1} = :parser.parse(tokens)
    {:ok, tokens, _} = :lexer.string(~c"Like anything")
    {:ok, ast2} = :parser.parse(tokens)

    assert {:error, :TypeError, errorMsg} = Core.typeList([ast1, ast2])

    assert PrettyPrint.printError({:error, :TypeError, errorMsg}) ==
             "TypeError:  TypeMismatch: Just Text AND: Text"
  end
end
