grammar Saki;

/* Parser Rules */

program: NL* (instructions+=instruction (NL+ instructions+=instruction)*)? NL* EOF;

moduleElement
    :
    ;

expr
    // Value
    :   value=atom                                                          # exprAtom
    |   func=expr '(' NL* argList NL* ')'                                   # exprCall
    |   func=expr '[' NL* argList NL* ']'                                   # exprImplicitCall
    |   '(' value=blockExpr ')'                                             # exprParen
    |   '\'(' elements+=expr ',' NL* elements+=expr ')'                     # exprTuple
    |   '^(' types+=expr ',' NL* types+=expr ')'                            # exprTupleType
    |   inductive=Identifier
        ('[' NL* indImplicitArgList=argList NL* ']')?
        ('(' NL* indExplicitArgList=argList NL* ')')?
        '::' constructor=Identifier                         # exprConstructor
    |   '(' paramList ')' (':' type=expr) '=>' body=blockExpr                   # exprLambda
    |   func=expr ('|' untypedParamList '|' (':' type=Identifier)?)? body=block  # exprCallWithLambda
    |   subject=expr '.' field=Identifier                                   # exprProjection
    |   lhs=expr rhs=expr                                                 # exprSeq
    // Control
    |   'if' NL* cond=blockExpr NL* 'then' NL* then=blockExpr NL* 'else' NL* else=blockExpr                    # exprIf
    |   'match' value=expr '{' NL* cases+=matchCase (NL+ cases+=matchCase)* NL* '}'                      # exprMatch
    // Types
    |   <assoc=right> lhs=expr '->' rhs=expr                                    # exprArrowType
    |   <assoc=right> '∀' '(' arg=expr ':' type=expr ')' '->' rhs=expr          # exprPiType
    |   <assoc=right> '∃' '(' arg=expr ':' type=expr ')' '->' rhs=expr          # exprSigmaType
    |   <assoc=right> '[' lhs=expr ']' '->' rhs=expr                            # exprImplicitArrowType
    |   <assoc=right> '∀' '[' arg=expr ':' type=expr ']' '->' rhs=expr          # exprImplicitPiType
    |   'record' (':' super=expr)? '{' NL* valueTypePair (NL+ valueTypePair)* NL* '}' # exprRecordType
    |   record=expr '@' '{' NL* fieldAssignment (NL+ fieldAssignment)* NL* '}' # exprRecord
    ;

blockExpr
    :   expr
    |   block
    ;

argList
    :   (args+=expr (',' NL* args+=expr)* ','?)?
    ;

matchCase
    :   'case' clauses+=matchClause (NL* '|' clauses+=matchClause)* NL* '=>' NL* value=expr
    ;

matchClause
    :   pattern (':' type=expr)?
    ;

pattern
    :   literal             # patternLiteral
    |   '`' value=expr '`'  # patternValue
    |   ident=Identifier    # patternVariable
    |   (inductive=Identifier
         ('(' NL* indExplicitArgList=argList NL* ')')?
         ('[' NL* indImplicitArgList=argList NL* ']')? '::')?
            constructor=Identifier ('(' NL* argList NL* ')')?   # patternConstructor
    |   '(' NL* patternList NL* ')' # patternTuple
    |   record=expr '@' '{' NL* (fields+=patternRecordField (',' NL* fields+=patternRecordField)* ','?)? NL* '}' # patternRecord
    ;

patternList
    :   patterns+=pattern (',' NL* patterns+=pattern)* ','?
    ;

patternRecordField
    :   ident=Identifier '=' value=pattern
    ;

instruction
    // Definition
    :   expr                                                                    # instructionExpr
    |   'let' name=Identifier (':' type=expr) '=' NL* value=blockExpr           # instructionLet
    |   'instance' ':' type=expr '=' NL* value=blockExpr                        # instructionInstance
    |   definition                                                              # instructionDef
    |   'impl' ('[' paramList ']')? type=expr
            '{' NL* (defs+=definition (NL+ defs+=definition)*)? NL* '}'         # instructionImpl
    |   operatorDeclaration                                                     # instructionOperator
    ;

block
    :   '{' NL* (instrs+=instruction (NL+ instructions+=instruction)*)? NL* '}'
    ;

definition
    :   'def' ident=definitionIdentifier ('[' implicitParamList=paramList ']')?
          ('(' explicitParamList=paramList ')')? ':' returnType=expr '=' NL* body=definitionBody    # defGeneral
    |   'type' ident=Identifier ('[' implicitParamList=paramList ']')?
          ('(' explicitParamList=paramList ')')? '=' NL* body=definitionBody            # defType
    ;

definitionIdentifier
    :   Identifier
    |   '(' OptSymbol ')'
    ;

definitionBody
    :   blockExpr                                                            # defBodyExpr
    |   'inductive' ('(' NL* inductiveParams+=Identifier (',' NL* inductiveParams+=Identifier)* ','? NL* ')')?
            NL* '{' NL* (cases+=inductiveCons (NL+ cases+=inductiveCons)*)? NL* '}' # defBodyInductive
    ;

inductiveCons
    :   isFlat='flat'? ident=Identifier ':' type=expr                                                          # inductiveConsType
    |   isFlat='flat'? ident=Identifier ('(' NL* (elements+=expr (',' NL* elements+=expr)* ','?)? NL* ')')?    # inductiveConsValue
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

untypedParamList
    :   (params+=untypedParam (',' params+=untypedParam)* ','?)?
    ;

untypedParam
    :   (idents+=Identifier)+ (':' type=expr)?
    ;

atom
    :   'self'              # atomSelf
    |   literal             # atomLiteral
    |   ident=Identifier    # atomIdentifier
    |   op=OptSymbol         # atomOperator
    ;

valueTypePair
    :   'self' (':' type=expr)?             # valueTypePairSelf
    |   (idents+=Identifier)+ ':' type=expr # valueTypePairTyped
    ;

fieldAssignment
    :   ident=Identifier '=' value=blockExpr
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

PiSymbol: '∀' | 'Π';
SigmaSymbol: '∃' | 'Σ';

Identifier: ValueIdent | TypeIdent | ContractIdent;

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

// camelCaseWithEnglishOrGreekLetters with optional postfix single quotation
fragment ValueIdent: [a-z]+([A-Za-zα-ωΑ-Ω])*('\'')*;

// A single blackboard bold letter or PascalCase
fragment TypeIdent: PascalCase | BlackboardBoldLetter;

// Quoted PascalCase: A PascalCase with a prefixed single quotation
fragment ContractIdent: '\''TypeIdent;

fragment PascalCase: [A-Z][a-zA-Z]*;

// Blackboard bold letters (𝔸 - 𝕫)
fragment BlackboardBoldLetter: [𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ];

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
