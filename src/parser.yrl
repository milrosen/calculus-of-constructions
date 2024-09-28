Nonterminals expr aexpr bexpr vexpr.
Terminals '(' ')' lambda pi '@' ':' arrow star box label number 'let' '=' 'in'.
Rootsymbol expr.

expr -> 'let' '(' label ':' expr ')' '=' expr 'in' expr : {app, {lam, get_token('$3'), '$5', '$10'}, '$8'}.
expr -> bexpr : '$1'.
expr -> lambda label arrow expr : {lam, get_token('$2'), {const, star}, '$4'}.
expr -> lambda '(' label ':' expr ')' arrow expr : {lam, get_token('$3'), '$5', '$8'}.
expr -> pi     '(' label ':' expr ')' arrow expr : {pi, get_token('$3'), '$5', '$8'}.
expr -> bexpr arrow expr : {pi, '_', '$1', '$3'}.

vexpr -> label '@' number : {v, get_token('$1'), get_token('$3')}.
vexpr -> label : {v, get_token('$1'), 0}.

bexpr -> bexpr aexpr :{app, '$1', '$2'}.
bexpr -> aexpr : '$1'.

aexpr -> vexpr : {var, '$1'}.
aexpr -> star : {const, get_const_token('$1')}.
aexpr -> box :  {const, get_const_token('$1')}.
aexpr -> '(' expr ')' : '$2'.

Erlang code.
get_token({_, _, Token}) -> Token.
get_const_token({Token, _})    -> Token.