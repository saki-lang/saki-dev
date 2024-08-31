grammar Saki;

/* Parser Rules */

startRule: NL* (expr (NL+ expr)*)? NL* EOF;

expr
    // Value
    :   atom                                                    # exprAtom
    |   '(' expr ')'                                            # exprParen
    |   expr '(' (expr (',' NL* expr)* ','?)? NL* ')'           # exprCall
    |   expr '.' Identifier                                     # exprField
    |   expr expr+                                              # exprSeq
    |   block                                                   # exprBlock
    // Control
    |   'if' expr 'then' expr 'else' expr                       # exprIf
    |   'match' expr '{' NL* (expr '=>' expr NL*)+ '}'          # exprMatch
    // Definition
    |   'let' Identifier '=' NL* expr                           # exprLet
    |   definition                                              # exprDef
    |   'impl' ('[' paramList ']')? expr '{' NL* (definition (NL+ definition)*)? NL* '}' # exprImpl
    // Types
    |   <assoc=right> expr '->' expr                                # exprFunctionType
    |   'enum' '{' NL* enumVariant (NL+ enumVariant)* NL* '}'       # exprEnum
    |   'struct' '{' NL* valueTypePair (NL+ valueTypePair)* NL* '}' # exprStruct
    ;

block
    :   '{' NL* (expr (NL+ expr)*)? NL* '}' # blockExpr
    ;

definition
    : 'def' Identifier ('[' paramList ']')? ('(' paramList ')')? (':' expr)? '=' NL* expr
    ;

paramList
    :   (valueTypePair (',' valueTypePair)* ','?)?
    ;

atom
    :   literal
    |   Identifier
    |   Operator
    |   '\'Type'
    ;

enumVariant
    :   Identifier
    |   Identifier '(' NL* (expr (',' NL* expr)* ','?)? NL* ')'
    |   Identifier '{' NL* (valueTypePair (',' NL* valueTypePair)* ','?)? NL* '}'
    ;

valueTypePair
    :   Identifier+ ':' expr
    ;

literal
    :   integral=(Dec | Hex | Oct | Bin)        # literalIntegral
    |   float=Float                             # literalFloat
    |   character=CharacterLiteral              # literalCharacter
    |   regularString=RegularStringLiteral      # literalRegularString
    |   rawString=RawStringLiteral              # literalRawString
    |   value=('true' | 'false')                # literalBoolean
    ;

// Literals
Dec: [0-9]+;
Hex: '0x' HexDigit+;
Oct: '0o' [0-7]+;
Bin: '0b' [01]+;
Float: [0-9]+[.][0-9]+;
Imaginary: [0-9]+[.][0-9]+ 'i';
CharacterLiteral: '\'' (~['\\\r\n] | CommonCharacter) '\'';
RegularStringLiteral: '"'  (~["\\\r\n] | CommonCharacter)* '"';
RawStringLiteral: '#' '{' (~["\\\r\n] | CommonCharacter)* '}';

Equal: '=';
RightArrow: '->';
RightDoubleArrow: '=>';

Operator: [+\-/*<>=|&!^%#:]+;

Identifier: [a-zA-Z_][a-zA-Z0-9_]*[']?;

// Whitespaces
Whitespace: [ \t\r]+ -> skip;

Comment
    : '/*' (Comment | .)*? '*/' -> skip
    ;

LineComment
    : '//' (~[\r\n])* -> skip
    ;

// NewLine
NL: '\n' | '\r' '\n'?;

/* Fragments */

fragment HexDigit: [0-9] | [A-F] | [a-f];
fragment ExponentPart: [eE] ('+' | '-')? [0-9] ('_'* [0-9])*;

fragment CommonCharacter
	: '\\\''
	| '\\"'
	| '\\\\'
	| '\\0'
	| '\\a'
	| '\\b'
	| '\\f'
	| '\\n'
	| '\\r'
	| '\\t'
	| '\\v'
	;

fragment NewLineAsciiChar
	: '\r\n' | '\r' | '\n'
	;
