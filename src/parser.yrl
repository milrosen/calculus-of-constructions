Nonterminals expr aexpr bexpr vexpr anno annos.
Terminals '(' ')' '{' '}' lambda pi '@' ':' arrow star box label number fun dot comma forall 'let' '=' 'in'.
Rootsymbol expr.

expr -> 'let' label ':' expr '=' expr 'in' expr : {'let', get_token('$2'), '$4', '$6', '$8'}.
expr -> bexpr : '$1'.
expr -> lambda annos arrow expr : {lam, '$2', '$4'}.
expr -> fun    annos dot   expr : {lam, '$2', '$4'}.
expr -> pi     annos arrow expr : {pi,  '$2', '$4'}.
expr -> forall anno  comma expr : {pi,  ['$2'], '$4'}.
expr -> '{' annos '}' expr      : {pi,  '$2', '$4'}.
expr -> bexpr arrow expr : {pi, '_', '$1', '$3'}.

vexpr -> label '@' number : {v, get_token('$1'), get_token('$3')}.
vexpr -> label : {v, get_token('$1'), 0}.

bexpr -> bexpr aexpr :{app, '$1', '$2'}.
bexpr -> aexpr : '$1'.

annos -> anno              : ['$1'].
annos -> anno comma annos  : ['$1' | '$3'].

anno -> '(' label ':' expr ')' : {get_token('$2'), '$4'}.
anno -> label ':' expr         : {get_token('$1'), '$3'}.
anno -> label                  : {get_token('$1'), hole}.

aexpr -> vexpr : {var, '$1'}.
aexpr -> star : {const, get_const_token('$1')}.
aexpr -> box :  {const, get_const_token('$1')}.
aexpr -> '(' expr ')' : '$2'.

Erlang code.
get_token({_, _, Token})    -> Token.
get_const_token({Token, _}) -> Token.