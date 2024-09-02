grammar Saki;

/* Parser Rules */

file: NL* (exprs+=expr (NL+ exprs+=expr)*)? NL* EOF;

expr
    // Value
    :   value=atom                                                          # exprAtom
    |   '(' value=expr ')'                                                  # exprParen
    |   '\'(' elements+=expr ',' NL* elements+=expr ')'                     # exprTuple
    |   '^(' types+=expr ',' NL* types+=expr ')'                            # exprTupleType
    |   func=expr '(' NL* (args+=expr (',' NL* args+=expr)* ','?)? NL* ')'  # exprCall
    |   subject=expr '.' member=Identifier                                  # exprField
    |   '|' paramList '|' body=expr                                         # exprLambda
    |   lhs=expr rhs=expr                                                   # exprSeq
    |   block                                                               # exprBlock
    // Control
    |   'if' NL* cond=expr NL* 'then' NL* then=expr NL* 'else' NL* else=expr                    # exprIf
    |   'match' value=expr '{' NL* cases+=matchCase (NL+ cases+=matchCase)* NL* '}'                      # exprMatch
    // Definition
    |   'let' name=Identifier (':' type=expr) '=' NL* value=expr                           # exprLet
    |   'instance' ':' type=expr '=' NL* value=expr     # exprInstance
    |   definition                                              # exprDef
    |   'impl' ('[' paramList ']')? expr '{' NL* (defs+=definition (NL+ defs+=definition)*)? NL* '}' # exprImpl
    // Types
    |   <assoc=right> lhs=expr '->' rhs=expr        # exprFunctionType
    |   <assoc=right> '<' lhs=expr '>' '->' rhs=expr        # exprFunctionTypeImplicit
    |   'enum' '{' NL* variants+=enumVariant (NL+ variants+=enumVariant)* NL* '}'       # exprEnumType
    |   'record' (':' super=expr)? '{' NL* valueTypePair (NL+ valueTypePair)* NL* '}' # exprRecordType
    |   record=expr '@' '{' NL* valueTypePair (NL+ valueTypePair)* NL* '}' # exprRecord
    ;

matchCase
    :   pattern=expr (':' type=expr)? '=>' value=expr
    |   '(' elements+=expr ',' NL* elements+=expr ')' '=>' value=expr
    ;

block
    :   '{' NL* (exprs+=expr (NL+ exprs+=expr)*)? NL* '}'
    ;

definition
    : 'def' ident=Identifier ('[' implicitParamList=paramList ']')?
        ('(' explicitParamList=paramList ')')? (':' returnType=expr)? '=' NL* body=expr
    ;

paramList
    :   (params+=valueTypePair (',' params+=valueTypePair)* ','?)?
    ;

atom
    :   literal             # atomLiteral
    |   ident=Identifier    # atomIdentifier
    |   op=Operator         # atomOperator
    |   '\'Type'            # atomType
    ;

enumVariant
    :   Identifier                                                                  # enumVariantSimple
    |   Identifier '(' NL* (elements+=expr (',' NL* elements+=expr)* ','?)? NL* ')'                     # enumVariantTuple
    |   Identifier '{' NL* (fields+=valueTypePair (',' NL* fields+=valueTypePair)* ','?)? NL* '}'   # enumVariantRecord
    ;

valueTypePair
    :   (idents+=Identifier)+ ':' type=expr
    ;

literal
    :   value=(Dec | Hex | Oct | Bin)       # literalInt
    |   value=Float                         # literalFloat
    |   value=CharacterLiteral              # literalChar
    |   value=RegularStringLiteral          # literalRegularString
    |   value=RawStringLiteral              # literalRawString
    |   value=('true' | 'false')            # literalBool
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
Colon: ':';

Operator: [+\-/*<>=&!^%#:]+;

Identifier: '_' | [a-zA-Z][a-zA-Z0-9_]*[']?;

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
