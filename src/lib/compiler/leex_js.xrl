
Definitions.

OpenMultilineComment  = \/\*
CloseMultiLineComment = \*\/
SingleLineComment     = \/\/

RegularExpressionLiteral = (\A\/|[^\d]\/)(\/\/|[^\/])+(\/\Z|\/[^\d])

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
Divide = \/
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

FloatLiteral = [+-]?(0|[1-9]+)(\.[0-9]+)([eE][+-][1-9]+)?

IntegerLiteral = [+-]?(0|[1-9]+)

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

{RegularExpressionLiteral} : operator('regexp', TokenLine, TokenChars).

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
{Divide} : operator('/', TokenLine).
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

{HtmlComment} : skip().
{CDataComment} : skip().

{NonPrintableCharacters} : skip().

{UnexpectedCharacter} : skip().

Erlang code.

operator(ID, TokenLine) -> {token, {ID, TokenLine}}.

operator(ID, TokenLine, TokenChars) -> {token, {ID, TokenLine, TokenChars}}.

null(TokenLine) -> {token, {'null', TokenLine}}.

bool("true", TokenLine) -> {token, {'true', TokenLine}};
bool("false", TokenLine) -> {token, {'false', TokenLine}}.

float(TokenChars, TokenLine) -> {token, {erlang:list_to_float(TokenChars), TokenLine}}.

integer(TokenChars, TokenLine) -> {token, {erlang:list_to_integer(TokenChars), TokenLine}}.


skip() -> skip_token.