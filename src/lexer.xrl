Definitions.

DIGIT = [0-9]+
WHITESPACE = [\s\n\t\r]
FST = [A-Za-z\_]
LABELCHAR = [A-Za-z0-9\_]
OPCHAR = [\!\$\%\&\*\+\/\<\=\>\?\@\\\^\|\-\~]

Rules.

\)           : {token, {')', TokenLine}}.
\(           : {token, {'(', TokenLine}}.
:            : {token, {':',  TokenLine}}.
\*           : {token, {star,   TokenLine}}.
@            : {token, {'@',    TokenLine}}.
BOX          : {token, {box,    TokenLine}}.
->           : {token, {arrow,  TokenLine}}.
\\\/         : {token, {pi,     TokenLine}}.
forall       : {token, {forall, TokenLine}}.
\\           : {token, {lambda, TokenLine}}.
let          : {token, {'let', TokenLine}}.
=            : {token, {'=',   TokenLine}}.
:=           : {token, {':=',  TokenLine}}.
in           : {token, {'in',  TokenLine}}.
fun          : {token, {'fun', TokenLine}}.
\,           : {token, {'comma', TokenLine}}.
\.           : {token, {'dot', TokenLine}}.
\{           : {token, {'{', TokenLine}}.
\}           : {token, {'}', TokenLine}}.
\#           : {token, {'#', TokenLine}}.


{FST}{LABELCHAR}*|\(\s?{OPCHAR}+\s?\) : {token, {label,  TokenLine, list_to_atom(TokenChars)}}.
{DIGIT}                               : {token, {number, TokenLine, list_to_integer(TokenChars)}}.

{WHITESPACE}+ : skip_token.
--.*          : skip_token.         

Erlang code.