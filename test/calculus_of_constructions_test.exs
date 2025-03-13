defmodule CalculusOfConstructionsTest do
  use ExUnit.Case
  doctest CalculusOfConstructions

  test "check program" do
    prog = CalculusOfConstructions.check("
    #check id (fun x : * . x)
    #def id := fun x : * -> * . x
    #check id (fun x : * . x)
    ")

    assert prog ==
             {:ok,
              [
                {:error, "Unbound Variable id in (id λ(x : *) → x) "},
                {:def, :id,
                 {:lam, :x, {:pi, :_, {:const, :star}, {:const, :star}}, {:var, {:v, :x, 0}}}},
                {:check, {:pi, :_, {:const, :star}, {:const, :star}}}
              ]}
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
    #check idid")

    assert match?({:ok, [_, _, {:check, _}]}, prog)
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
