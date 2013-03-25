// for now, this is a grammar for rust token-trees.
// it's still missing numeric constants and all kinds of comments.
grammar Rust;

import "xidstart" , "xidcont";

prog : tts;

tts : tt* ;
tt : nondelim | delimited ;
delimited : LPAREN tt* RPAREN
  | LBRACKET tt* RBRACKET
  | LBRACE tt* RBRACE ;
nondelim : path
    // putting in keywords to simplify things:
  | AS
  | ASSERT
  | BREAK
  | CONST
  | COPY
  | DO
  | DROP
  | ELSE
  | ENUM
  | EXTERN
  | FALSE
  | FN
  | FOR
  | IF
  | IMPL
  | LET
  | __LOG
  | LOOP
  | MATCH
  | MOD
  | MUT
  | ONCE
  | PRIV
  | PUB
  | PURE
  | REF
  | RETURN
  | STRUCT
  | SUPER
  | TRUE
  | TRAIT
  | TYPE
  | UNSAFE
  | USE
  | WHILE
  // Expression-operator symbols.
  |  EQ
  |  LT
  |  LE
  |  EQEQ
  |  NE
  |  GE
  |  GT
  |  ANDAND
  |  OROR
  |  NOT
  |  TILDE
  |  BINOP
  |  BINOPEQ
  // Structural symbols 
  |  AT
  |  DOT
  |  DOTDOT
  |  COMMA
  |  SEMI
  |  COLON
  |  MOD_SEP
  |  RARROW
  |  LARROW
  |  DARROW
  |  FAT_ARROW
  |  POUND
  |  DOLLAR
  // Literals 
  |  LIT_INT
  //|  LIT_UINT
  //|  LIT_INT_UNSUFFIXED
  |  LIT_FLOAT
  //|  LIT_FLOAT_UNSUFFIXED
  |  LIT_STR
  // Name components 
  |  IDENT
  |  UNDERSCORE
  |  LIFETIME
  // For interpolation
  // |  INTERPOLATED
  |  DOC_COMMENT ;

path : MOD_SEP? IDENT (MOD_SEP IDENT)* ;

// putting keywords in to simplify things:
AS : 'as' ;
ASSERT : 'assert' ;
BREAK : 'break' ;
CONST : 'const' ;
COPY : 'copy' ;
DO : 'do' ;
DROP : 'drop' ;
ELSE : 'else' ;
ENUM : 'enum' ;
EXTERN : 'extern' ;
FALSE : 'false' ;
FN : 'fn' ;
FOR : 'for' ;
IF : 'if' ;
IMPL : 'impl'   ;
LET : 'let' ;
__LOG : '__log' ;
LOOP : 'loop' ;
MATCH : 'match' ;
MOD : 'mod' ;
MUT : 'mut' ;
ONCE : 'once' ;
PRIV : 'priv' ;
PUB : 'pub' ;
PURE : 'pure' ;
REF : 'ref' ;
RETURN : 'return' ;
STRUCT : 'struct' ;
SUPER : 'super' ;
TRUE : 'true' ;
TRAIT : 'trait' ;
TYPE : 'type' ;
UNSAFE : 'unsafe' ;
USE : 'use' ;
WHILE : 'while' ;

EQ     : '=' ;
LE     : '<=' ;
LT     : '<';
EQEQ : '==' ;
NE   : '!=' ;
GE   : '>=' ;
GT   : '>' ;
ANDAND : '&&' ;
OROR  : '||' ;
NOT   : '!' ;
TILDE : '~' ;
BINOP : '<<' | '>>' 
      | '*' | [-&|+/^%] ;
BINOPEQ : BINOP '=' ;
/* Structural symbols */
AT        : '@' ;
DOT       : '.' ;
DOTDOT    : '..' ;
COMMA     : ',' ;
SEMI      : ';' ; 
COLON     : ':' ;
MOD_SEP   : '::' ;
RARROW    : '->' ;
LARROW    : '<-' ;
DARROW    : '<->' ;
FAT_ARROW : '=>' ;
LPAREN    : '(' ;
RPAREN    : ')' ;
LBRACKET  : '[' ;
RBRACKET  : ']' ;
LBRACE    : '{' ;
RBRACE    : '}' ;
POUND     : '#' ;
DOLLAR    : '$' ;

// includes both INT and UINT INT_UNSUFFIXED
LIT_INT   : LIT_CHAR
          | '0x' HEXDIGIT+ INTLIT_TY?
          | '0b' BINDIGIT+ INTLIT_TY?
          | DECDIGIT+ INTLIT_TY?
          ;

// we may need lookahead here; the rust lexer
// checks whether the char following the . is
// alpha, dot, or _, and bails if so. Wait... why
// doesn't it just check that there's at least one
// digit? Perhaps because of the underscore restriction?
LIT_FLOAT : DECDIGIT+ '.'
          // nb: digit following . can't be underscore.
          | DECDIGIT+ '.' [0-9] DECDIGIT* LITFLOAT_EXP? LITFLOAT_TY?
          | DECDIGIT+ LITFLOAT_EXP LITFLOAT_TY?
          | DECDIGIT+ LITFLOAT_TY
          ;

LIT_STR : '\"' STRCHAR* '\"' ;
IDENT : IDSTART IDCONT* ;
UNDERSCORE : '_' ;

// there's potential ambiguity with char constants,
// but I think that the greedy read will do the "right
// thing"
LIFETIME : '\'' IDENT ;
// the not-only-slashes restrictions is a real PITA:
// must have at least one non-slash char
DOC_COMMENT : '///' '/' * NON_SLASH_OR_WS ~[\n]*
  | '///' '/' * [ \r\n\t] ~[ \r\n\t] ~[\n]* 
  | '//!' ~[\n]*
    // again, we have to do a funny dance to fence out
    // only-stars.
    // CAN'T ABSTRACT OVER BLOCK_CHARS; learned this
    // the hard way ... :(
  | '/**' (~[\*] | ('*' ~[/]))* ~[*] (~[\*] | ('*' ~[/]))* '*/' 
  | '/*!'(~[\*] | ('*' ~[/]))* '*/' ;

// HELPER DEFINITIONS:

WS : [ \t\r\n]+ -> skip ; // skip spaces, tabs, newlines
OTHER_LINE_COMMENT : '//' ~[\n] * -> skip ;
OTHER_BLOCK_COMMENT : '/*' (~[\*] | ('*' ~[/]))* '*/' -> skip ;


// strangely, underscores are allowed anywhere in these?
BINDIGIT : [0-1_] ;
DECDIGIT : [0-9_] ;
HEXDIGIT : [0-9a-fA-F_] ;
INTLIT_TY : ('u'|'i') ('8'|'16'|'32'|'64') ;
LITFLOAT_EXP : [eE] [+-]? DECDIGIT+ ;
LITFLOAT_TY : 'f' ('32'|'64') ;

ESCAPEDCHAR : 'n' | 'r' | 't' | '\\' | '\'' | '\"'
  | 'x' HEXDIGIT HEXDIGIT | 'u' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT
  | 'U' HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT HEXDIGIT
            ;

LIT_CHAR :  '\'\\' ESCAPEDCHAR '\'' | '\'' . '\'' ;

STRCHAR : ~[\\\"] | '\\' STRESCAPE ;
STRESCAPE : '\n' | ESCAPEDCHAR ;

IDSTART : [_a-zA-Z]  | XIDSTART ;
IDCONT : [_a-zA-Z0-9] | XIDCONT ;

NON_SLASH_OR_WS : ~[ \t\r\n/] ;
