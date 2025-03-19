Nonterminals canno cannos labels triples triple expr aexpr bexpr vexpr anno annos program sentence.
Terminals '#' '(' ')' '{' '}' lambda pi '@' ':' '|' system arrow star box label number fun dot comma forall 'let' '=' ':=' 'in'
def with eval typeof check.
Rootsymbol program.

program -> sentence : ['$1'].
program -> sentence program : ['$1' | '$2'].

sentence -> '#' with annos : {get_const_token('$2'), '$3'}.
sentence -> '#' def label ':=' expr : {get_const_token('$2'), get_token('$3'), '$5'}.
sentence -> '#' eval expr : {get_const_token('$2'), '_', '$3'}.
sentence -> '#' typeof expr : {get_const_token('$2'), '_', '$3'}.
sentence -> '#' check label ':' expr ':=' expr : {get_const_token('$2'), get_token('$3'), '$5', '$7'}.
sentence -> '#' system labels '|' cannos '|' triples : {get_const_token('$2'), '$3', '$5', '$7'}.
sentence -> expr : '$1'.

labels -> label : [get_token('$1')].
labels -> label labels : [get_token('$1') | '$2'].

triple -> '(' label label label ')' : {get_token('$2'), get_token('$3'), get_token('$4')}.
triples -> triple : ['$1'].
triples -> triple triples : ['$1' | '$2'].

expr -> 'let' label ':' expr '=' expr 'in' expr : {'let', get_token('$2'), '$4', '$6', '$8'}.
expr -> bexpr : '$1'.
expr -> lambda annos arrow expr : {lam, '$2', '$4'}.
expr -> fun    annos dot   expr : {lam, '$2', '$4'}.
expr -> pi     annos arrow expr : {pi,  '$2', '$4'}.
expr -> forall anno  comma expr : {pi,  ['$2'], '$4'}.
expr -> '{' annos '}' expr      : {pi,  '$2', '$4'}.
expr -> bexpr arrow expr : {pi, [{'_', '$1'}], '$3'}.

vexpr -> label '@' number : {v, get_token('$1'), get_token('$3')}.
vexpr -> label : {v, get_token('$1'), 0}.

bexpr -> bexpr aexpr :{app, '$1', '$2'}.
bexpr -> aexpr : '$1'.

annos -> anno              : ['$1'].
annos -> anno comma annos  : ['$1' | '$3'].

cannos -> canno : ['$1'].
cannos -> canno cannos : ['$1' | '$2'].

canno -> label ':' label : {get_token('$1'), get_token('$3')}.

anno -> '(' label ':' expr ')' : {get_token('$2'), '$4'}.
anno -> label ':' expr         : {get_token('$1'), '$3'}.
anno -> label                  : {get_token('$1'), hole}.


aexpr -> vexpr : {var, '$1'}.
aexpr -> star : {const, get_const_token('$1')}.
aexpr -> box :  {const, get_const_token('$1')}.
aexpr -> number : {const, {number, get_token('$1')}}.
aexpr -> '(' expr ')' : '$2'.

Erlang code.
get_token({_, _, Token})    -> Token.
get_const_token({Token, _}) -> Token.