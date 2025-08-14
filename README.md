# CalculusOfConstructions

An implementation of the family of programming languages from [this](https://homepages.inf.ed.ac.uk/wadler/papers/barendregt/pure-type-systems.pdf) paper.


```tikz
\usepackage{tikz-cd}
\begin{document}
\begin{tikzcd}[row sep=3.4em]
    & \lambda\omega \arrow[rr] \arrow[from=dd] && \lambda C \\
    \lambda \textbf{2} \arrow[ur] \arrow[rr, crossing over] && \lambda \textbf{P2} \arrow[ur] & \\
	& \lambda\underline{\omega} \arrow[rr] && \lambda \textbf{P} \underline{\omega} \arrow[uu] \\
	\lambda\rightarrow \arrow[uu] \arrow[rr] \arrow[ur] && \lambda \textbf{P} \arrow[ur] \arrow[uu, crossing over]
\end{tikzcd}

\end{document}
```

In this diagram, eight systems are defined. The arrow represents $\subseteq$, so the weakest system is the simply typed lambda calculus, at the bottom left. Other named systems are $\lambda 2$, which is equivalent to system $\textbf{F}$, which is the polymorphic lambda calculus. $\lambda\omega$ is equivalent to $F\omega$, which was proposed by Girard, $\lambda P$ is equivalent to the "automath" language, and is sometimes called $LF$, which is the language that beluga uses for "weak functions" allowing a form of impredicativity without generating inconsistencies or even large cardinals.

The underlines are pronounced "weak" (like the weak functions from LF). P2 is studied under the name P2. (Is this the "pure" type system?)

## Kinds of dependence

There are four kinds of dependency at play (but only three directions?)
- **Terms depending on Terms** ($\star \leadsto \star$)
- **Terms depending on Types** $(\Box \leadsto \star)$
- **Types depending on Terms** $(\star \leadsto \Box)$
- **Types depending on Types** ($\Box \leadsto \Box$)
(these are, presumably the $\Box \leadsto \star$ style rules  from the Henk paper)

Terms depending on terms is very common, any function application, $F\ x$, is an example.

Types depending on terms is complex, imagine a type indexed by a natural number, in the paper this is $A^n \rightarrow B$ (so the functions of the nth A to B), could be defined as:
- $A^0 \rightarrow B = B$ 
- $A^{n+1} \rightarrow B = A \rightarrow (A^n \rightarrow B)$ 
So here $A$ is always the type, just the type that depends on $n$ (are all of these types equal to each other, do they all normalize to the same term?)

Types depending on types is just normal polymorphism, like how 'a-s work in OCaml (are there weak and strong versions of this? what's the difference?)

Types depending on terms has us introduce the cartesian product. So 
$$\lambda x: A . b_a$$
has type
$$\Pi a : A . B_a$$
Where $B_a$ may or may not depend on $a$. Note that if it doesn't, like if $B_a = B_a'$ for every $a:A$ $a' : A$, then this dependent product is just exactly the set $B^A$, which is exactly the set of all functions $A \rightarrow B$ (isn't that lovely)
(what does it mean for terms to depend on types?)

## Box and Star

In a simply typed system, types aren't part of the language, and are given in the metalanguage. (with a few constructors like Arrow or maybe List to enable compound types, but all the atomic types must be metalanguage)

In non simple systems (which ones?) it makes sense to add introducing new types as part of the formal system itself. For this, we need to extend the $:$ judgement. It is now the "kinding" judgement, so the "kind" of a term is its type, but the *kind* of a type is $\star$. We also extend our $\vdash$ to say what kind of new types. So instead of being a separate rule for the typing judgement, or part of the metalanguage, I can write that $\Gamma, A : \star \vdash A \rightarrow A : \star$ (or, if a is a type, then so is a function from $A$ to $A$). 
\usetheme frankfurt, \colortheme seahorse
Next, we can reintroduce our dependencies from earlier, now as elements of the formal language itself:

$$
\begin{align}
 (\lambda m : A . F\ m) &: (A \rightarrow B) \\
 (\lambda \alpha : \star . \mathbf{I}_\alpha) &: (\Pi\alpha : \star . \alpha \rightarrow \alpha)  \\
(\lambda n : \mathbb{N} . A^n \rightarrow B) &: (\mathbb{N} \rightarrow \star) \\
(\lambda \alpha : \star . \alpha \rightarrow \alpha) &: (\star \rightarrow \star)
\end{align} \\
$$
In this case, the first one is a term depending on a term. It's just a function. The next one is a dependent product version of $A \rightarrow A$, where $\alpha$ might appear in $\mathbf{I}$. The next one is a dependent type. In the previous example, the $\textbf{I}_\alpha$ meant that alpha could appear in $\textbf{I}$. Does the $A^n$ mean the same thing here? Like, is it just that the term $A$ can depend on $n$?

However, now we can ask the question: what is the sort/kind of the $(\star \rightarrow \star)$? Surely, it cant be a $\star$ itself, cause that would lead to all sorts of problems. Instead, we introduce a higher kind, $\Box$. We say that $(\star \rightarrow \star) : \Box$. 

On $\Pi$. The expression: $(\Pi \alpha : \star . (\alpha \rightarrow \alpha))$, since it is a product over types, is also a type. So, $\vdash (\Pi \alpha : \star . (\alpha \rightarrow \alpha)) : \star$. The inhabitants of $\star \rightarrow \star$ are called "constructors" (this is where $\texttt{list}$ lives, awaiting its $\texttt{'a}$). 

# Definitions

First, we define a set of terms:
$$\def\b{\mathrel{|}} \mathfrak{T} :=x \b c \b \mathfrak{T}\ \mathfrak{T} \b \Pi x : \mathfrak{T}\ \mathfrak{T}$$
Where $x$ are the variables, and $c$ are the constants $\Box$, $\star$. 
In a statement of $A : B$, $A$ is the *subject*  and $B$ is the *predicate*. 
$\Gamma$ is a context, it is a list of variable-predicate pairs, and the $\vdash$ symbol is an inductive relation on contexts and predicates, defined according to the rules:

$$
\renewcommand\arraystretch{3}\begin{array}{lpc}
Axiom && \vdash \star \\
Start &&\dfrac{\Gamma \vdash A :s}{\Gamma, x : A \vdash x :A} \\
Weakening && \dfrac{\Gamma \vdash A : B \quad \Gamma \vdash C : s}
{\Gamma, x : C\vdash A : B} \\
Application && \dfrac{\Gamma \vdash F : (\Pi x : A. B) \quad \Gamma \vdash a : A}{\Gamma \vdash Fa : [a/x]B} \\
Conversion && \dfrac{\Gamma \vdash A : B \quad \Gamma \vdash B' : s \quad B =_\beta B'}{\Gamma A : B'}
\end{array}
$$

Now, for the  next two of rules, they are parameterized by a set of two symbols, $(s_1, s_2)$. Each of the systems of the lambda cubes are determined by replacing the $s_1$ and $s_2$ with $(\star, \Box$) for instance. 
$$
\begin{array}{lc}
\Pi{-}rule & \dfrac{\Gamma \vdash A : s_1\quad \Gamma, x : A \vdash B : s_2}{\Gamma \vdash (\Pi x : A . B) : s_2} \\
\lambda{-}rule &\dfrac{\Gamma \vdash A : s_1\quad \Gamma, x : A \vdash b : B\quad \Gamma, x : A \vdash B : s_2}{\Gamma (\lambda x: A .b) : (\Pi x : A . B)}
\end{array}
$$
The eight systems of the lambda cube are defined by taking the general rules and adding rule pairs from the set $\{(\star, \star), (\star, \Box), (\Box, \star), (\Box, \Box)\}$ 
$$
\begin{array}{l|cccc}
\lambda\rightarrow & (\star, \star)\\
\lambda \textbf{2} & (\star, \star) & (\Box, \star) \\
\lambda \underline\omega & (\star, \star) && (\Box, \Box) \\
\lambda \omega& (\star, \star) & (\Box, \star) & (\Box, \Box) \\
\lambda\textbf{P} & (\star, \star) &&& (\star, \Box)\\
\lambda \textbf{P2} & (\star, \star) & (\Box, \star)&& (\star, \Box) \\
\lambda \textbf{P}\underline\omega & (\star, \star) && (\Box, \Box)& (\star, \Box) \\
\lambda \textbf{P}\omega& (\star, \star) & (\Box, \star) & (\Box, \Box)&(\star, \Box) \\

\end{array}
$$
note that $\lambda \textbf{P}\omega = \lambda C$, and remember that $\lambda \textbf{P}$ is the LF
For some more definitions:
- in $\Gamma \vdash A : B$ then $A$ and $B$ are (legal) terms and $\Gamma$ is the a (legal) context
- $\Gamma \vdash A : B : \star$, $A$ is an *object* and $B$ is a *type* 
- $\Gamma \vdash A : B : \Box$, A is a *constructor* and $B$ a *kind*
Every legal term is either an object, type, constructor, or kind. All types are also constructors. All corners of the cube have Church-Rosner, (multistep preserves beta equality, and determinism) Preservation (stepping terms are the same type) Normalization (all terms have or are values) Deterministic typing (all possible types of a term are beta equivalent)

# Examples
in $\lambda \rightarrow$, 
$$
\begin{align}
A : \star &\vdash (A \rightarrow A ) : \star\\
A : \star &\vdash (\lambda a: A.a) : (A \rightarrow A)\\
A : \star, B : \star, b : B &\vdash (\lambda a:A.b) : (A \rightarrow B)\\
A : \star, b : A &\vdash ((\lambda a : A.a)\ b) : A\\
A : \star, B : \star, c : A, b : B &\vdash ((\lambda a : A. b) c) : B
\end{align}
$$
In $\lambda \mathbf{2}$,
$$
\begin{align}
\alpha : \star &\vdash (\lambda a : \alpha .a) : (\alpha \rightarrow \alpha) \\
&\vdash (\lambda \alpha : \star . \lambda a : \alpha .a) : (\Pi \alpha : \star . (\alpha \rightarrow \alpha)) : \star\\
A : \star &\vdash (\lambda \alpha : \star .\lambda a : \alpha . a) A : (A \rightarrow A)
\end{align}
$$
	This system is "connected" to second order propositional logic, to see how, consider the following formula: $$ \vdash (\lambda\beta : \star . \lambda a : (\Pi \alpha : \star.\alpha). a ((\Pi \alpha : \star. \alpha) \rightarrow \beta)a):(\Pi \beta : \star . (\Pi \alpha : \star . \alpha ) \rightarrow \beta)$$It's easy to see that $\Pi\alpha:\star.\alpha$ is equivalent to the proposition $\forall P. P$ in second order logic, so replacing $\Pi\alpha:\star.\alpha$ with $\bot$, gives us: $$\vdash (\lambda \beta:\star.\lambda a:\bot . a\beta):(\Pi \beta : \star.\bot \rightarrow \beta) $$ My guess is that the below term has some kind of type-variable inference, not sure how to implement that, but it clearly isn't just using the abbreviation. 

in $\lambda\underline{\omega}$ 
	$$
	\begin{align}
	\vdash & (\lambda \alpha:\star . \alpha \rightarrow \alpha) : (\star \rightarrow \star) : \Box \\
	\beta : \star \vdash& (\lambda \alpha : \star .\alpha \rightarrow \alpha )\beta : \star\\
	\beta : \star, x : \beta \vdash& (\lambda y :\beta.x ) : (\lambda\alpha : \star . \alpha \rightarrow \alpha)\beta \\
	\end{align}
	$$
	note that $\lambda y : \beta . x$ has type $\beta \rightarrow \beta$, and not some other, higher type. We don't have $\Pi$, and so cant abstract over type arguments, they need to be instantiated in our context before they can show up on the right of the colon$$
	\begin{align}
	\alpha : \star, f : \star \rightarrow \star \vdash& f(f\ \alpha) : \star\\
	\alpha : \star \vdash & (\lambda f : \star \rightarrow \star . f(f\ \alpha)) : (\star \rightarrow \star ) \rightarrow \star
	\end{align}
	$$ Next, observe the parameterized $\lambda$-rule in action, since $\star \rightarrow \star : \Box$, and $f \vdash f(f\ \alpha) : \star : \Box$, we can use the rule with $s_1, s_2 = (\Box, \Box)$. 

in $\lambda \textbf{P}$ (LF)$$ 
	\begin{align}
	A : \star \vdash &  (A \rightarrow \star) : \Box
	\end{align}
	$$
	If the type $A$ is a set, then $A \rightarrow \star$ is the kind of predicates on $A$. Dependent types are what lets us do predicates, so in beluga we have $o : \star$ is the type of formulas, $o \rightarrow \star$ is the type of $A\ \texttt{true}$. $$
	\begin{align}
	A : \star, P : (A \rightarrow \star), a : A \vdash P\ a : \star
	\end{align} 
	$$$P\ a$ is the predicate applied to $a$, it is inhabited if $P$ is true of $a$. 
	 Then, we use the prod rule to get: $$ A : \star, P : A \rightarrow \star \vdash (\Pi a : A. P\ a) : \star$$ So this is the proposition that $\forall a \in A . P a$, it's true if the type is inhabited.$$\begin{array}{l}
	A : \star,& P: (A\rightarrow \star), Q : \star \\
	& \vdash ((\Pi a:A.Pa \rightarrow Q) \rightarrow (\Pi:A.Pa) \rightarrow Q) \\
	A : \star,& P: (A\rightarrow \star), Q : \star, a_0 : A \\
	&\vdash (\lambda x : (\Pi a : A.Pa\rightarrow Q)\qquad\ \lambda y:(\Pi a : A. Pa). xa_0 (ya_0)) :\\
	&\quad (\Pi x : (\Pi a : A.Pa\rightarrow Q) \rightarrow (\Pi y : (\Pi a. :A . Pa).\quad Q \quad)) \equiv \\
	&\qquad\quad \ \ (\Pi a : A. Pa \rightarrow Q) \rightarrow \qquad\ \ (\Pi a. :A.Pa). \quad Q
	\end{array} $$
	A proof of the claim that $(\forall x \in A . Pa \rightarrow Q) \rightarrow (\forall a \in A. Pa) \rightarrow Q)$ 
	The last step is because $\Pi x : A . B \equiv A \rightarrow B$ when $x$ doesn't appear in $B$ .  

in $\lambda \omega$. 
	First, let $\alpha \wedge \beta \equiv\Pi\gamma : \star . (\alpha \rightarrow \beta \rightarrow \gamma) \rightarrow \gamma$ 	$$\alpha : \star, \beta : \star \vdash \alpha \wedge \beta : \star$$This is the second-order definition of $\&$, it is *definable* in $\lambda 2$, while the *term* $$AND : (\star \rightarrow \star \rightarrow \star) \equiv \lambda \alpha : \star. \lambda \beta : \star . \Pi \gamma : \star . (\alpha \rightarrow \beta \rightarrow \gamma) \rightarrow \gamma$$and requires product types (since its most general type should have a variable)(not dependent types, just first-class polymorphic types $(\lambda 2)$, $\Pi \alpha : \star . \alpha \rightarrow \alpha$) *and* the ability to have type constructors ($\lambda\underline{\omega}$). However, this isn't dependent types since these types cannot contain any terms. 
	Next, we define $K : (\Pi \alpha : \star . \Pi \beta : \star . \alpha \rightarrow \beta \rightarrow \alpha) \equiv \lambda\alpha:\star \lambda\beta:\star\lambda x:\alpha\lambda y : \beta . x$ Which is the polymorphic "ignore the second argument" function. 
	Then, we can have the same argument that 

in $\lambda \textbf{P2}$ (second order predicate logic)
	$$\begin{align}
	A : \star, P : (A \rightarrow \star) \vdash& (\lambda a : A. Pa \rightarrow \bot) : A \rightarrow \star\\
	A : \star, P : (A \rightarrow A \rightarrow \star) \vdash& 
	\big[(\Pi a : A\ \Pi b : A. P ab \rightarrow P ba \rightarrow \bot) \rightarrow (\Pi a : A, Paa \rightarrow \bot) \big] : \star
	\end{align}$$
	The second proposition states that $P$ is neither symmetric nor reflexive
in $\lambda \textbf{P}\underline{\omega}$ 
	$$A : \star \vdash (\lambda P :(A \rightarrow A \rightarrow \star) \lambda a : A . P aa) : ((A \rightarrow A \rightarrow \star) \rightarrow (A \rightarrow \star)) : \Box$$
	This is the constructor of a diagonalization of a predicate


in $\lambda\textbf{P}\omega = \lambda \textbf{C}$ 
	 if we define $\textbf{ALL} \equiv (\lambda A : \star\lambda P : A \rightarrow \star.\Pi a:A.Pa)$ then,$$A : \star, P :(A \rightarrow \star) \vdash \textbf{ALL}\ A\ P : \star \mathrel{\text{and}} (
		\textbf{ALL}\ A\ P) =_\beta (\Pi a : A. Pa)$$The book calls this "universal quantification done uniformly"
	 I do not know what that means.

