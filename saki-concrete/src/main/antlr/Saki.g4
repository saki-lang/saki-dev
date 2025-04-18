grammar Saki;

/* Parser Rules */

program
    : NL* (entities+=moduleEntity (NL+ entities+=moduleEntity NL*)* NL*)? NL* EOF
    ;

moduleEntity
    :   definition                                                              # moduleEntityDef
    |   'impl' ('[' NL* paramList NL* ']')? type=expr NL*
            '{' NL* (defs+=definition (NL+ defs+=definition)*)? NL* '}'         # moduleEntityImpl
    |   operatorDeclaration                                                     # moduleEntityOpDecl
    |   'eval' value=blockExpr                                                  # moduleEntityEval
    ;

expr
    // Value
    :   value=atom                                                                                      # exprAtom
    |   func=expr '(' NL* argList NL* ')'                                                               # exprCall
    |   func=expr '[' NL* argList NL* ']'                                                               # exprImplicitCall
    |   subject=expr NL* '.' member=Identifier ('[' implicitArgList=argList ']')?                       # exprElimination
    |   inductive=expr '::' constructor=Identifier                                                      # exprConstructor
    |   expr '_' Dec                                                                                    # exprTupleProjection
    |   ('^'|'typeof') '(' expr ')'                                                                     # exprTypeOf
    |   lhs=expr rhs=atom                                                                               # exprSpine
    |   lhs=expr op=OptSymbol rhs=expr                                                                  # exprSpineInfixOp
    |   lhs=expr op=OptSymbol                                                                           # exprSpinePostfixOp
    |   op=OptSymbol rhs=expr                                                                           # exprSpinePrefixOp
    |   types+=atom ('|' types+=atom)+                                                                  # exprUnionType
    |   types+=atom ('&' types+=atom)+                                                                  # exprIntersectionType
    |   '(' value=blockExpr ')'                                                                         # exprParen     // TODO: also exist in `atom`, consider remove this rule
    |   '\'(' elements+=expr ',' NL* elements+=expr ')'                                                 # exprTuple
    |   '(' types+=expr ',' NL* types+=expr ')'                                                         # exprTupleType
    |   '(' NL* lambdaParamList=paramList NL* ')' (':' returnType=expr)? '=>' body=blockExpr            # exprLambda
    |   func=expr ('|' lambdaParamList=untypedParamList '|' (':' returnType=expr)?)? body=block         # exprCallWithLambda
    // Control
    |   'if' NL* cond=blockExpr NL* 'then' NL* then=blockExpr NL* 'else' NL* else=blockExpr             # exprIf
    |   'match' value=expr '{' NL* cases+=matchCase (NL+ cases+=matchCase)* NL* '}'                     # exprMatch
    // Types
    |   <assoc=right> domain=expr '->' codomain=expr                                                    # exprArrowType
    |   <assoc=right> '[' domain=expr ']' '->' codomain=expr                                            # exprImplicitArrowType
    |   <assoc=right> '∀' '(' NL* param=Identifier ':' domain=expr NL* ')' '->' codomain=expr           # exprPiType
    |   <assoc=right> '∀' '[' NL* param=Identifier ':' domain=expr NL* ']' '->' codomain=expr           # exprImplicitPiType
    |   <assoc=right> '∃' '(' NL* param=Identifier ':' domain=expr NL* ',' codomain=expr ')'            # exprSigmaType
    |   'record' (':' super=expr)? '{' NL* fields+=identTypePair (NL+ fields+=identTypePair)* NL* '}'   # exprRecordType
    |   recordType=expr '\'' '{' NL* fields+=fieldAssignment (NL+ fields+=fieldAssignment)* NL* '}'     # exprRecord
    ;

blockExpr
    :   expr        # blockExprExpr
    |   block       # blockExprBlock
    ;

argList
    :   (args+=expr (',' NL* args+=expr)* ','?)?
    ;

matchCase
    :   'case' clauses+=matchClause (NL* '|' clauses+=matchClause)* NL* '=>' NL* body=blockExpr
    ;

matchClause
    :   pattern (':' type=expr)?                        # matchClauseSingle
    |   '(' NL* patternList NL* ')' (':' type=expr)?    # matchClauseTuple
    ;

pattern
    :   literal                         # patternLiteral
    |   '`' value=expr '`'              # patternValue
    |   ident=(Identifier|WildCard)     # patternVariable
    |   inductive=expr '::' constructor=Identifier ('(' NL* consPatternList=patternList NL* ')')?   # patternConstructor
    |   '(' NL* patternList NL* ')'                                                                 # patternTuple
    |   record=expr '\'' '{' NL* (fields+=patternRecordField (',' NL* fields+=patternRecordField)* ','?)? NL* '}' # patternRecord
    ;

patternList
    :   patterns+=pattern (',' NL* patterns+=pattern)* ','?
    ;

patternRecordField
    :   ident=Identifier '=' value=pattern
    ;

statement
    // Definition
    :   expr                                                                        # statementExpr
    |   blockExpr                                                                   # statementBlock
    |   'let' name=(WildCard|Identifier) (':' type=expr)? '=' NL* value=blockExpr   # statementLet
    |   'instance' ':' type=expr '=' NL* value=blockExpr                            # statementInstance
    ;

block
    :   '{' NL* (statements+=statement (NL+ statements+=statement)*)? NL* '}'
    ;

definition
    :   'def' ident=definitionIdentifier ('[' NL* implicitParamList=paramList NL* ']')?
          ('(' NL* explicitParamList=paramList NL* ')')? (':' returnType=expr)? '=' NL* body=definitionBody # defGeneral
    |   'type' ident=Identifier ('[' NL* implicitParamList=paramList NL* ']')?
          ('(' NL* explicitParamList=paramList NL* ')')? '=' NL* body=definitionBody                        # defType
    ;

definitionIdentifier
    :   string=Identifier               # defIdentString
    |   '(' operator=OptSymbol ')'      # defIdentOperator
    ;

definitionBody
    :   blockExpr   # defBodyExpr
    |   'inductive' ('(' NL* inductiveParams+=Identifier (',' NL* inductiveParams+=Identifier)* ','? NL* ')')?
            NL* '{' NL* (constructors+=inductiveCons (NL+ constructors+=inductiveCons)*)? NL* '}' # defBodyInductive
    ;

inductiveCons
    :   isFlat='flat'? ident=Identifier ':' type=expr   # inductiveConsType
    |   isFlat='flat'? ident=Identifier
            ('(' NL* (elements+=inductiveTupleElement (',' NL* elements+=inductiveTupleElement)* ','?)? NL* ')')?   # inductiveConsTuple
    ;

inductiveTupleElement
    :   (ident=Identifier ':')? type=expr
    ;

operatorDeclaration
    :   'operator' 'binary' '('symbol=OptSymbol')' associativity=('left-assoc' | 'right-assoc')
        ('{' NL* operatorPrecedence (NL+ operatorPrecedence)* NL* '}')?  # binaryOperator
    |   'operator' symbol=OptSymbol 'unary' kind=('prefix' | 'postfix') # unaryOperator
    ;

operatorPrecedence
    :   (   'tighter-than' ('('tighterThan+=OptSymbol')')+
        |   'looser-than' ('('looserThan+=OptSymbol')')+
        |   'same-as' ('('sameAs+=OptSymbol')')+
        )
    ;

paramList
    :   (params+=identTypePair (',' NL* params+=identTypePair)* ','?)?
    ;

untypedParamList
    :   (params+=untypedParam (',' params+=untypedParam)* ','?)?
    ;

untypedParam
    :   ident=Identifier (':' type=expr)?
    ;

atom
    :   'self'                  # atomSelf
    |   literal                 # atomLiteral
    |   ident=Identifier        # atomIdentifier
    |   '(' op=OptSymbol ')'    # atomOperator
    |   '(' expr ')'            # atomParen
    ;

identTypePair
    :   (idents+=Identifier)+ ':' type=expr
    ;

fieldAssignment
    :   ident=Identifier '=' value=blockExpr
    ;

literal
    :   value=Int                   # literalInt
    |   value=Float                 # literalFloat
    |   value=CharacterLiteral      # literalChar
    |   value=RegularStringLiteral  # literalRegularString
    |   value=RawStringLiteral      # literalRawString
    |   value=('true' | 'false')    # literalBool
    ;

// Literals
Int: '-'? (Dec | Hex | Oct | Bin);
Dec: [0-9]+;
Hex: '0x' HexDigit+;
Oct: '0o' [0-7]+;
Bin: '0b' [01]+;
Float: '-'?[0-9]+[.][0-9]+;
Imaginary: [0-9]+[.][0-9]+ 'i';
CharacterLiteral: '\'' (~['\\\r\n] | CommonCharacter) '\'';
RegularStringLiteral: '"'  (~["\\\r\n] | CommonCharacter)* '"';
RawStringLiteral: '#' '{' (~["\\\r\n] | CommonCharacter)* '}';

Equal: '=';
RightArrow: '->';
RightDoubleArrow: '=>';
Colon: ':';

LineComment
    : '//' (~[\r\n])* -> skip
    ;

OptSymbol: [+\-/*<>=&!^%#:]+ | '||';

PiSymbol: '∀' | 'Π';
SigmaSymbol: '∃' | 'Σ';

Identifier: ValueIdent | TypeIdent | ContractIdent;
WildCard: '_';

// Whitespaces
Whitespace: [ \t\r]+ -> channel(HIDDEN);

Comment
    : '/*' (Comment | .)*? '*/' -> skip
    ;

// NewLine
NL: '\n' | '\r' '\n'? | ';';

/* Fragments */

// camelCaseWithEnglishOrGreekLetters with optional postfix single quotation
fragment ValueIdent: [a-zα-ω]+([A-ZΑ-Ωa-zα-ω0-9])*('\'')*;

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
