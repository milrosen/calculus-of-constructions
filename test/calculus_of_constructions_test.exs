defmodule CalculusOfConstructionsTest do
  use ExUnit.Case
  doctest CalculusOfConstructions

  test "check program" do
    prog = CalculusOfConstructions.check("
    #type id (fun x : * . x)
    #def id := fun x : * -> * . x
    #type id (fun x : * . x)
    ")

    assert prog ==
             {:ok,
              [
                {:error, "Unbound Variable id"},
                {:def, :id,
                 {:lam, :x, {:pi, :_, {:const, :star}, {:const, :star}}, {:var, {:v, :x, 0}}}},
                {:type, {:pi, :_, {:const, :star}, {:const, :star}}}
              ]}
  end

  test "check program with" do
    {:ok, [{:with, _}, {:check, :f, claim, body}]} = CalculusOfConstructions.check("
    #with A : *, B : *
    #check f : A -> B -> B := fun a : A, b : B . b
    ")

    assert claim ==
             {:pi, :_, {:var, {:v, :A, 0}}, {:pi, :_, {:var, {:v, :B, 0}}, {:var, {:v, :B, 0}}}}

    assert body ==
             {:lam, :a, {:var, {:v, :A, 0}}, {:lam, :b, {:var, {:v, :B, 0}}, {:var, {:v, :b, 0}}}}
  end

  test "lexer error" do
    prog = CalculusOfConstructions.check("??")

    assert match?({:error, _}, prog)
  end

  test "parser error" do
    prog = CalculusOfConstructions.check("#def id : {A : *} fun a : A . a")

    assert match?({:error, _}, prog)
  end

  test "nested definitions" do
    prog = CalculusOfConstructions.check("
    #def id := fun x : Nat . x
    #def idid := fun x : Nat . id x
    #type idid")

    assert match?({:ok, [_, _, {:type, _}]}, prog)
  end

  test "parenthesis" do
    {:ok, [{:eval, ast1, _}, {:def, :"(+)", ast2}]} = CalculusOfConstructions.check("
    #eval succ (succ (succ (succ zero)))
    #def (+) := fun a : Nat, b : Nat .
      indNat a (fun _ : Nat . Nat) b
      (fun _ : Nat, pm1 : Nat . succ pm1)")

    assert {PrettyPrint.printExpr(ast1), PrettyPrint.printExpr(ast2)} ==
             {"(succ (succ (succ (succ zero))))",
              "λ(a : Nat) → λ(b : Nat) → indNat a (λ(_ : Nat) → Nat) b (λ(_ : Nat) → λ(pm1 : Nat) → (succ pm1))"}
  end
end
