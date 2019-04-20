/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2019 by Vjacheslav Brichkovskiy (original author)
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

Definitions.

OpenMultilineComment  = \/\*
CloseMultiLineComment = \*\/
SingleLineComment     = \/\/

RegularExpressionLiteral = /(?:)/

OpenBracket = \[
CloseBracket = \]
OpenParen = \(
CloseParen = \)
OpenBrace = \{
CloseBrace = \}
Semicolon = ;
Comma = ,
Assign = =
QuestionMark = \?
Colon = :
Ellipsis = ...
Dot = .
PlusPlus = \+\+
MinusMinus = --
Plus = \+
Minus = -
BitNot = ~
Not = !
Multiply = \*
Divide = /
Modulus = %
RightShiftArithmetic = >>
LeftShiftArithmetic = <<
RightShiftLogical = >>>
LessThan = <
MoreThan = >
LessThanEquals = <=
GreaterThanEquals = >=
Equals = ==
NotEquals = !=
IdentityEquals = ===
IdentityNotEquals = !==
BitAnd = &
BitXOr = \^
BitOr = \|
And = &&
Or = \|\|
MultiplyAssign = *=
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

Erlang code.

operator(ID, TokenLine) -> {token, {ID, TokenLine}}.

operator(ID, TokenLine, TokenChars) -> {token, {ID, TokenLine, TokenChars}}.