grammar Saki;

/* Parser Rules */

program: NL* (stmts+=stmt (NL+ stmts+=stmt)*)? NL* EOF;

expr
    // Value
    :   value=atom                                                          # exprAtom
    |   '(' value=expr ')'                                                  # exprParen
    |   '\'(' elements+=expr ',' NL* elements+=expr ')'                     # exprTuple
    |   '^(' types+=expr ',' NL* types+=expr ')'                            # exprTupleType
    |   func=expr '(' NL* (args+=expr (',' NL* args+=expr)* ','?)? NL* ')'  # exprCall
    |   subject=expr '.' member=Identifier                                  # exprMemberAccess
    |   record=expr '#' field=Identifier                                    # exprFieldProjection
    |   enum=Identifier ('[' implicitParamList=paramList ']')? ('(' explicitParamList=paramList ')')? '::' variant=Identifier # exprEnumValue
    |   '|' paramList '|' '=>'? body=expr                                   # exprLambda
    //|   lhs=expr rhs=expr                                                 # exprSeq
    |   block                                                               # exprBlock
    // Control
    |   'if' NL* cond=expr NL* 'then' NL* then=expr NL* 'else' NL* else=expr                    # exprIf
    |   'match' value=expr '{' NL* cases+=matchCase (NL+ cases+=matchCase)* NL* '}'                      # exprMatch
    // Types
    |   <assoc=right> lhs=expr '->' rhs=expr        # exprFunctionType
    |   <assoc=right> '<' lhs=expr '>' '->' rhs=expr        # exprFunctionTypeImplicit
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

stmt
    // Definition
    :   expr                                                                    # stmtExpr
    |   'let' name=Identifier (':' type=expr) '=' NL* value=expr                # stmtLet
    |   'instance' ':' type=expr '=' NL* value=expr                             # stmtInstance
    |   definition                                                              # stmtDef
    |   'impl' ('[' paramList ']')? type=expr
            '{' NL* (defs+=definition (NL+ defs+=definition)*)? NL* '}'         # stmtImpl
    |   isOpen='open' 'enum' ident=Identifier
            ('[' implicitParamList=paramList ']')? ('(' explicitParamList=paramList ')')?
            '{' NL* variants+=enumVariant (NL+ variants+=enumVariant)* NL* '}'  # stmtEnum
    |   operatorDeclaration                                                     # stmtOperator
    ;

block
    :   '{' NL* (stmts+=stmt (NL+ stmts+=stmt)*)? NL* '}'
    ;

definition
    :   'def' ident=(Identifier|OptSymbol) ('[' implicitParamList=paramList ']')?
          ('(' explicitParamList=paramList ')')? ':' returnType=expr '=' NL* body=expr
    ;

operatorDeclaration
    :   'operator' 'binary' symbol=OptSymbol associativity=('left-assoc' | 'right-assoc')
        ('{' NL* operatorPrecedence (NL+ operatorPrecedence) NL* '}')?  # binaryOperator
    |   'operator' symbol=OptSymbol 'unary' kind=('prefix' | 'postfix') # unaryOperator
    ;

operatorPrecedence
    :   (   'tighter-than' tighterThan+=OptSymbol+
        |   'looser-than' looserThan+=OptSymbol+
        |   'same-as' sameAs+=OptSymbol+
        )
    ;

paramList
    :   (params+=valueTypePair (',' params+=valueTypePair)* ','?)?
    ;

atom
    :   literal             # atomLiteral
    |   ident=Identifier    # atomIdentifier
    |   op=OptSymbol         # atomOperator
    ;

enumVariantData
    :   '(' NL* (elements+=expr (',' NL* elements+=expr)* ','?)? NL* ')'                # enumVariantDataTuple
    |   '{' NL* (fields+=valueTypePair (',' NL* fields+=valueTypePair)* ','?)? NL* '}'  # enumVariantDataRecord
    ;

enumVariant
    :   ident=Identifier data=enumVariantData?
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

Identifier: '_' | ['a-zA-Z][a-zA-Z0-9_]*[']?;

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
