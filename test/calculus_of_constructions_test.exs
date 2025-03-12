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
                {:check, "Unbound Variable id in (id λ(x : *) → x) "},
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
end
