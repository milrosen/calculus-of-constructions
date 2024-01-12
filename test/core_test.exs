defmodule CoreTests do
  use ExUnit.Case
  doctest CalculusOfInductiveTypes

  test "doesn't shift bound variables" do
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

  test "substitutes variable" do
    {:ok, tokens, _} = :lexer.string('f B')
    {:ok, ast} = :parser.parse(tokens)

    e = CalculusOfInductiveTypes.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:app, {:var, {:v, :f, 0}}, {:var, {:v, :y, 0}}}
  end

  test "substitutes variable in lambda" do
    {:ok, tokens, _} = :lexer.string('\\(x : *) -> B')
    {:ok, ast} = :parser.parse(tokens)

    e = CalculusOfInductiveTypes.subst(:B, 0, {:var, {:v, :y, 0}}, ast)

    assert e == {:lam, :x, {:const, :star}, {:var, {:v, :y, 0}}}
  end

  test "only subst free variables" do
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
end
