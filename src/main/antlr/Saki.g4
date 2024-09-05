grammar Saki;

/* Parser Rules */

program: NL* (exprs+=expr (NL+ exprs+=expr)*)? NL* EOF;

expr
    // Value
    :   value=atom                                                          # exprAtom
    |   '(' value=expr ')'                                                  # exprParen
    |   '\'(' elements+=expr ',' NL* elements+=expr ')'                     # exprTuple
    |   '^(' types+=expr ',' NL* types+=expr ')'                            # exprTupleType
    |   func=expr '(' NL* (args+=expr (',' NL* args+=expr)* ','?)? NL* ')'  # exprCall
    |   subject=expr '.' member=Identifier                                  # exprMemberAccess
    |   record=expr '::' field=Identifier                                 # exprFieldProjection
    |   '|' paramList '|' body=expr                                         # exprLambda
    //|   lhs=expr rhs=expr                                                   # exprSeq
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
    |   record=expr '@' '{' NL* fieldAssignment (NL+ fieldAssignment)* NL* '}' # exprRecord
    ;

matchCase
    :   pattern (':' type=expr)? '=>' NL* value=expr
    ;

pattern
    :   literal             # patternLiteral
    |   ident=Identifier    # patternVariable
    |   '(' elements+=pattern (',' NL* elements+=pattern)* ','? ')' # patternTuple
    |   ident=Identifier '(' NL* (elements+=pattern (',' NL* elements+=pattern)* ','?)? NL* ')' # patternVariant
    |   record=expr '@' '{' NL* (fields+=patternRecordField (',' NL* fields+=patternRecordField)* ','?)? NL* '}' # patternRecord
    ;

patternRecordField
    :   ident=Identifier '=' value=pattern
    ;

block
    :   '{' NL* (exprs+=expr (NL+ exprs+=expr)*)? NL* '}'
    ;

definition
    :   'def' ident=Identifier ('[' implicitParamList=paramList ']')?
          ('(' explicitParamList=paramList ')')? ':' returnType=expr '=' NL* body=expr
    ;

operatorDefinition
    :   'def' 'binary' symbol=OptSymbol associativity=('left-assoc' | 'right-assoc')
        ('<' NL* operatorPrecedence (',' NL* operatorPrecedence) NL* '>')?
        ('[' implicitParamList=paramList ']')? '(' explicitParamList=paramList ')'
        (':' returnType=expr)? '=' NL* body=expr   # binaryOperator
    |   'def' 'unary' symbol=OptSymbol kind=('prefix' | 'postfix')
        ('[' implicitParamList=paramList ']')? '(' explicitParamList=paramList ')'
        (':' returnType=expr)? '=' NL* body=expr    # unaryOperator
    ;

operatorPrecedence
    :   (   'tighter' tighterThan+=OptSymbol+
        |   'looser' looserThan+=OptSymbol+
        |   'same' sameAs+=OptSymbol+
        )
    ;

paramList
    :   (params+=valueTypePair (',' params+=valueTypePair)* ','?)?
    ;

atom
    :   literal             # atomLiteral
    |   ident=Identifier    # atomIdentifier
    |   op=OptSymbol         # atomOperator
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

fieldAssignment
    :   ident=Identifier '=' value=expr
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

OptSymbol: [+\-/*<>=&!^%#:]+;

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
