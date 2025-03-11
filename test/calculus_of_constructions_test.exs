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
end
