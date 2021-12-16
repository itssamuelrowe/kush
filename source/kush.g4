compilationUnit
:   importDeclaration*
    componentDeclarations*
    EOF
;

componentDeclarations
:   functionDeclaration
|   structureDeclaration
;

importDeclaration
:   'import' qualifiedName SEMICOLON
;

qualifiedName
:   IDENTIFIER ('.' IDENTIFIER)*
;

functionDeclaration
:   returnType IDENTIFIER functionParameters functionBody
;

functionParameters
:   '(' functionParameterList? ')'
;

functionParameterList
:   functionParameter (',' functionParameter)* (',' variableFunctionParameter)?
|   variableFunctionParameter
;

functionParameter
:   type IDENTIFIER
;

variableFunctionParameter
:   type '...' IDENTIFIER
;

functionBody
:   blockStatement
;

blockStatement
:   '{' statement* '}'
;

statement
:   simpleStatement
|   compoundStatement
;

simpleStatement
:   unterminatedSimpleStatement SEMICOLON
;

unterminatedSimpleStatement
:   expressionStatement
|   emptyStatement
|   storageDeclaration
|   breakStatement
|   returnStatement
|   throwStatement
;

expressionStatement
:   expression
;

emptyStatement
:   ';'
;

storageDeclaration
:   ('var' | 'let' | type) storageDeclarator (',' storageDeclarator)*
;

storageDeclarator
:   IDENTIFIER ('=' expression)?
;

breakStatement
:   'break' IDENTIFIER?
;

returnStatement
:   'return' expression
;

throwStatement
:   'throw' expression
;

compoundStatement
:   ifStatement
|   iterativeStatement
|   tryStatement
;

ifStatement
:   ifClause elseIfClause* elseClause?
;

ifClause
:   'if' expression blockStatement
;

elseIfClause
:   'else' 'if' expression blockStatement
;

elseClause
:   'else' blockStatement
;

iterativeStatement
:   labelClause? (whileStatement | forStatement)
;

labelClause
:   '#' IDENTIFIER
;

whileStatement
:   'while' expression blockStatement
;

forStatement
:   'for' 'var' IDENTIFIER ':' expression blockStatement
;

tryStatement
:   tryClause catchClause* finallyClause?
;

tryClause
:    'try' blockStatement
;

catchClause
:    catch' (IDENTIFIER | STRING_LITERAL)? IDENTIFIER blockStatement
;

finallyClause
:	'finally' blockStatement
;

structureDeclaration
:   'struct' IDENTIFIER structureBody
;

structureBody
:   '{' structureMembers? '}'
;

structureMembers
:   type IDENTIFIER (SEMICOLON type IDENTIFIER)*
;

structureMember
:   type IDENTIFIER SEMICOLON
;

type
:   componentType ('[' ']')*
;

componentType
:   IDENTIFIER
|   'boolean',
|   'i8'
|   'i16',
|   'i32',
|   'i64',
|   'f32',
|   'f64'
;

returnType
:   'void'
|   type
;

expressions
:    expression (',' expression)*
;

expression
:	assignmentExpression
;

assignmentExpression
:	conditionalExpression (assignmentOperator assignmentExpression)?
;

assignmentOperator
:    '='
|    '*='
|    '/='
|    '%='
|    '+='
|    '-='
|    '<<='
|    '>>='
|    '>>>='
|    '&='
|    '^='
|    '|='
;

conditionalExpression
:	condition ('?' expression ':' conditionalExpression)?
;

condition
:	logicalAndExpression ('||' logicalAndExpression)*
;

logicalAndExpression
:	inclusiveOrExpression ('&&' logicalAndExpression)?
;

inclusiveOrExpression
:	exclusiveOrExpression ('|' exclusiveOrExpression)*
;

exclusiveOrExpression
:	andExpression ('^' andExpression)*
;

andExpression
:	equalityExpression ('&' equalityExpression)*
;

equalityExpression
:	relationalExpression (equalityOperator relationalExpression)*
;

equalityOperator
:	'=='
|	'!='
;

relationalExpression
:	shiftExpression (relationalOperator shiftExpression)*
;

relationalOperator
:	'<'
|	'>'
|	'<='
|	'>='
;

shiftExpression
:	additiveExpression (shiftOperator additiveExpression)*
;

shiftOperator
:	'<<'
|	'>>'
;

additiveExpression
:	multiplicativeExpression (multiplicativeOperator multiplicativeExpression)*
;

additiveOperator
:	'+'
|	'-'
;

multiplicativeExpression
:	unaryExpression (multiplicativeOperator unaryExpression)*
;

multiplicativeOperator
:	'*'
|	'/'
|	'%'
;

unaryExpression
:	unaryOperator unaryExpression
|	postfixExpression
;

unaryOperator
:    '+'
|    '-'
|    '~'
|    '!'
;

postfixExpression
:	primaryExpression postfixPart*
;

postfixPart
:    subscript
|    functionArguments
|    memberAccess
;

subscript
:	'[' expression ']'
;

functionArguments
:	'(' expressions? ')'
;

memberAccess
:	'.' IDENTIFIER
;

primaryExpression
:	IDENTIFIER
|	literal
|	'(' expression ')'
|	listExpression
|   constructorExpression
|   closureExpression
;

constructorExpression
:   '{' memberAssignments? '}'
;

memberAssignments
:   memberAssignment (',' memberAssignment)*
;

memberAssignment
:   IDENTIFIER ':' expression
;

literal
:	INTEGER_LITERAL
|	FLOATING_POINT_LITERAL
|	'true'
|	'false'
|	STRING_LITERAL
|	'null'
|   'this'
;

listExpression
:    '[' expressions ']'
;

newExpression
:    'new' typeName functionArguments
;

closureExpression
:   '@' closureParameters? closureBody
;

closureParameters
:   '(' IDENTIFIER (',' IDENTIFIER)+ ')'
|   IDENTIFIER
;

closureBody
:   '->' expression
|   blockStatement
;

/*
The various forms of a closure are given below.
```
@ -> expression
@argument -> expression
@(argument1, argument2) -> expression
@{
    statement1
    ...
    statementN
}
@argument {
    statement1
    ...
    statementN
}

@(argument1, argument2) {
    statement1
    ...
    statementN
}
```
*/