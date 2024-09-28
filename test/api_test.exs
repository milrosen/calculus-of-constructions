defmodule ApiTest do
  require Core
  require CalculusOfInductiveTypes
  use ExUnit.Case
  doctest CalculusOfInductiveTypes

  test "end to end" do
    proof = [
      ~c"let (mp : forall(P:*) -> forall(Q:*) -> (P -> Q) -> P -> Q) =
                  \\(P:*) -> \\(Q:*) -> \\(h: forall(p : P) -> Q) -> \\(hp: P) -> h hp in result",
      "false"
    ]

    {:ok, _} = CalculusOfInductiveTypes.typeProof(proof)
  end
end
