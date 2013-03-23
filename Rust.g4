// for now, this is a grammar for rust token-trees.
// it's still missing numeric constants and all kinds of comments.
grammar Rust;

import "xidstart" , "xidcont";

prog : tt* ;
tt : nondelim | delimited ;
delimited : LPAREN tt* RPAREN
          | LBRACKET tt* RBRACKET
          | LBRACE tt* RBRACE ;
nondelim :
  // Expression-operator symbols.
     EQ
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
  |  LIT_UINT
  |  LIT_INT_UNSUFFIXED
  |  LIT_FLOAT
  |  LIT_FLOAT_UNSUFFIXED
  |  LIT_STR
  // Name components 
  |  IDENT
  |  UNDERSCORE
  |  LIFETIME
  // For interpolation
  // |  INTERPOLATED
  |  DOC_COMMENT 
    ;
    
WS : [ \t\r\n]+ -> skip ; // skip spaces, tabs, newlines

EQ     : '=' ;
LE     : '<=' ;
LT     : '<';
/*
LE,
*/
EQEQ : '==' ;
NE   : '!=' ;
GE   : '>=' ;
GT   : '>' ;
ANDAND : '&&' ;
OROR  : '||' ;
NOT   : '!' ;
TILDE : '~' ;
BINOP : '<<' | '>>' 
      | [-&|+*/^%];
/*
BINOPEQ(binop),
*/
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

LIT_INT   : '\'\\' ESCAPEDCHAR '\'' | '\'' . '\'' ;
/*

// Literals
LIT_INT(i64, ast::int_ty),
LIT_UINT(u64, ast::uint_ty),
LIT_INT_UNSUFFIXED(i64),
LIT_FLOAT(ast::ident, ast::float_ty),
LIT_FLOAT_UNSUFFIXED(ast::ident),
*/

LIT_STR : '\"' STRCHAR * '\"' ;
IDENT : IDSTART IDCONT * ;
UNDERSCORE : '_' ;

// there's potential ambiguity with char constants,
// but I think that the greedy read will do the "right
// thing"
LIFETIME : '\'' IDENT ;
//    DOC_COMMENT(ast::ident),

HEX : [0-9a-fA-F] ;

ESCAPEDCHAR : 'n' | 'r' | 't' | '\\' | '\'' | '\"'
            | 'x' HEX HEX | 'u' HEX HEX HEX HEX
            | 'U' HEX HEX HEX HEX HEX HEX HEX HEX
            ;

STRCHAR : ~[\\\"] | '\\' STRESCAPE ;
STRESCAPE : '\n' | ESCAPEDCHAR ;

IDSTART : [_a-zA-Z]  | XIDSTART ;
IDCONT : [_a-zA-Z0-9] | XIDCONT ;
