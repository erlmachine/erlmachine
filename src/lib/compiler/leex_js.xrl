
Definitions.

OpenMultilineComment  = \/\*
CloseMultiLineComment = \*\/
SingleLineComment     = \/\/

OpenBracket = \[
CloseBracket = \]
OpenParen = \(
CloseParen = \)
OpenBrace = \{
CloseBrace = \}
Semicolon = ;
Comma = \,
Assign = \=
QuestionMark = \?
Colon = \:
Ellipsis = \.\.\.
Dot = \.
PlusPlus = \+\+
MinusMinus = \--
Plus = \+
Minus = \-
BitNot = \~
Not = \!
Multiply = \*

%% Note (To do carry out divide and regex operators on syntax level)
%% 1. Do context analysis. What does is left? For regex only operators available, for divide only values or values returned operators
%% 2. Does it is open regex slash? If is it, to do analysis for closing slash and flags (optionally)
Slash = \/

Modulus = \%
RightShiftArithmetic = \>>
LeftShiftArithmetic = \<<
RightShiftLogical = \>>>
LessThan = \<
MoreThan = \>
LessThanEquals = \<=
GreaterThanEquals = \>=
Equals = \==
NotEquals = \!=
IdentityEquals = \===
IdentityNotEquals = \!==
BitAnd = &
BitXOr = \^
BitOr = \|
And = &&
Or = \|\|
MultiplyAssign = \*=
DivideAssign = \/=
ModulusAssign = %=
PlusAssign = \+=
MinusAssign = -=
LeftShiftArithmeticAssign = <<=
RightShiftArithmeticAssign = >>=
RightShiftLogicalAssign = >>>=
BitAndAssign = &=
BitXOrAssign = \^=
BitOrAssign = \|=
Arrow = =>

NullLiteral = null

BooleanLiteral = true|false

%% Note (To do carry out operators +/- on syntax level)

FloatLiteral = (0|[1-9]+)(\.[0-9]+)([eE][+-][1-9]+)?

IntegerLiteral = (0|[1-9]+)

%% Note (Erlang does not support exponent notation at integer)

ExponentIntegerLiteral = (0|[1-9]+)([eE][+-][1-9]+)

HexIntegerLiteral = 0[xX][0-9a-fA-F]+
BinaryIntegerLiteral = 0[bB][0-1]+
OctalIntegerLiteral = 0[oO][0-7]+

NonPrintableCharacters =  [\s\t\f\r\n\e\v]+

HtmlComment = <\!--.*?-->
CDataComment = <\!\[CDATA\[.*?\]\]>

UnexpectedCharacter = .

Rules.

{OpenMultilineComment} : operator('/*', TokenLine).
{CloseMultiLineComment} : operator('*/', TokenLine).
{SingleLineComment} : operator('//', TokenLine).

{OpenBracket} : operator('[', TokenLine).
{CloseBracket} : operator(']', TokenLine).
{OpenParen} : operator('(', TokenLine).
{CloseParen} : operator(')', TokenLine).
{OpenBrace} : operator('{', TokenLine).
{CloseBrace} : operator('}', TokenLine).
{Semicolon} : operator(';', TokenLine).
{Comma} : operator(',', TokenLine).
{Assign} : operator('=', TokenLine).
{QuestionMark} : operator('?', TokenLine).
{Colon} : operator(':', TokenLine).
{Ellipsis} : operator('...', TokenLine).
{Dot} : operator('.', TokenLine).
{PlusPlus} : operator('++', TokenLine).
{MinusMinus} : operator('--', TokenLine).
{Plus} : operator('+', TokenLine).
{Minus} : operator('-', TokenLine).
{BitNot} : operator('~', TokenLine).
{Not} : operator('!', TokenLine).
{Multiply} : operator('*', TokenLine).
{Slash} : operator('/', TokenLine).
{Modulus} : operator('%', TokenLine).
{RightShiftArithmetic} : operator('>>', TokenLine).
{LeftShiftArithmetic} : operator('<<', TokenLine).
{RightShiftLogical} : operator('>>>', TokenLine).
{LessThan} : operator('<', TokenLine).
{MoreThan} : operator('>', TokenLine).
{LessThanEquals} : operator('<=', TokenLine).
{GreaterThanEquals} : operator('>=', TokenLine).
{Equals} : operator('==', TokenLine).
{NotEquals} : operator('!=', TokenLine).
{IdentityEquals} : operator('===', TokenLine).
{IdentityNotEquals} : operator('!==', TokenLine).
{BitAnd} : operator('&', TokenLine).
{BitXOr} : operator('^', TokenLine).
{BitOr} : operator('|', TokenLine).
{And} : operator('&&', TokenLine).
{Or} : operator('||', TokenLine).
{MultiplyAssign} : operator('*=', TokenLine).
{DivideAssign} : operator('/=', TokenLine).
{ModulusAssign} : operator('%=', TokenLine).
{PlusAssign} : operator('+=', TokenLine).
{MinusAssign} : operator('-=', TokenLine).
{LeftShiftArithmeticAssign} : operator('<<=', TokenLine).
{RightShiftArithmeticAssign} : operator('>>=', TokenLine).
{RightShiftLogicalAssign} : operator('>>>=', TokenLine).
{BitAndAssign} : operator('&=', TokenLine).
{BitXOrAssign} : operator('^=', TokenLine).
{BitOrAssign} : operator('|=', TokenLine).
{Arrow} : operator('=>', TokenLine).

{NullLiteral} : null(TokenLine).

{BooleanLiteral} : bool(TokenChars, TokenLine).

{FloatLiteral} : float(TokenChars, TokenLine).

{IntegerLiteral} : integer(TokenChars, TokenLine).

{HexIntegerLiteral} : hexinteger(TokenChars, TokenLine).
{BinaryIntegerLiteral} : binaryinteger(TokenChars, TokenLine).
{OctalIntegerLiteral} : octalinteger(TokenChars, TokenLine).

{HtmlComment} : skip().
{CDataComment} : skip().

{NonPrintableCharacters} : skip().

{UnexpectedCharacter} : skip().

Erlang code.

operator(ID, TokenLine) -> io:format("~nOperator: ~p~n",[ID]), {token, {ID, TokenLine}}.

null(TokenLine) -> io:format("~nnull~n"), {token, {'null', TokenLine}}.

bool("true", TokenLine) -> io:format("~ntrue~n"), {token, {'true', TokenLine}};
bool("false", TokenLine) -> io:format("~nfalse~n"), {token, {'false', TokenLine}}.

float(TokenChars, TokenLine) -> io:format("~nFloat: ~p~n",[TokenChars]), {token, {erlang:list_to_float(TokenChars), TokenLine}}.

integer(TokenChars, TokenLine) -> io:format("~nInteger: ~p~n",[TokenChars]), {token, {erlang:list_to_integer(TokenChars), TokenLine}}.

hexinteger([_, _|TokenChars], TokenLine) -> io:format("~nhexinteger: ~p~n",[TokenChars]), {token, {erlang:list_to_integer(TokenChars, 16), TokenLine}}.
binaryinteger([_, _|TokenChars], TokenLine) -> io:format("~nbinaryinteger: ~p~n",[TokenChars]), {token, {erlang:list_to_integer(TokenChars, 2), TokenLine}}.
octalinteger([_,_|TokenChars], TokenLine) -> io:format("~noctalinteger: ~p~n",[TokenChars]), {token, {erlang:list_to_integer(TokenChars, 8), TokenLine}}.


skip() -> skip_token.