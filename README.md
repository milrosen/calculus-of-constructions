# CalculusOfConstructions

An implementation of the family of programming languages from [this](https://homepages.inf.ed.ac.uk/wadler/papers/barendregt/pure-type-systems.pdf) paper. You can read my notes on the paper [here](milrosen.github.io/portfolio-v4/blog/pure-type-systems). This is just a personal project, so the scope is very small. Currently, it is a sucessful implementation of the generalized PTSs defined in that paper. I would like to add recursive types, but that currently seems beyond me, and would almost certainly require a full rewrite. 

# Language

Each line of code is a command followed by some optional arguments. These arguments can be names of functions, definitions of a specific PTS, or just some code to exec/type. The current commands are:

## Commands

- **#def** *name* := *expr*: all subsequent terms will be type checked with *name* as *expr*. Since we do not check the term, this is usefull for defining terms like False := $\Pi A . A$, which are not typed by themselves. This does not add the term to context, so even with false defined, it would not be well typed when used later. 

- **#with** *name* : *type* *: Adds variables of a type to the typing context. This is usefull for adding terms that our language cannot define by itself, like adding $A$ and $B$ as types to the STLC

- **#check** *name* : *term* := *term* normalizes the term and type and then checks that they are syntactically equivalent (note: this means that $A \rightarrow A \not= B \rightarrow B$, but $\Pi A : \star . A = \Pi B : \star . B$). If they are, the term is defined and added to the typing context

- **#typeof** *term*: types the term and prints the type if it exists

- **#eval** *term*: types the term and prints the normalized version of the term if the type exists

- **#system** *sorts* | *axioms* | *rules*: defines a pts with the specified sorts, axioms and rules. "star," "box" and "triangle" will be displayed as $\star$, $\Box$ and $\Delta$, but every other sort is just a non-unicode string. Without a system command, every term is typed with the full $\lambda$C rules

For more information about what these commands actually mean, read the paper or notes from the beginning.

## Term Syntax

Lambda terms can be written `\a : A -> b`, or `fun a : A . b` 

Pi terms can be written `\/a : A -> B`, `forall a : A, B`, `{a : A} B`, or `A -> B` for the non-dependent type. 

Since we don't unify, there is no difference between `{a : A} B` and `forall a : A, B`.

Both Lambda and Pi terms are curried by default, but Pi terms written with the `forall` syntax are not (this is personal preference).

Applications are written `f a` for `f(a)`

# Some technical details (for me)

We do not have delta substitutions or spines in our core language, so the named-environment and currying is handled in a separate desugaring step, this has proven clunky when it comes to adding new commands.

