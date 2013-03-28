grammar Rust;

import "xidstart" , "xidcont";

// parsing a whole file as a 'tts' should work on current sources.
tts : tt* ;

// parsing a whole file as a 'prog' will not yet work; 
prog : inner_attr* mod_item*;

// for now, lumping together all kinds of items: this will
// be changing soon anyway.

// incomplete! :
mod_item : outer_attrs (PUB | PRIV)? mod_item_2 ;
mod_item_2 : const_item
    // ...
  | enum_item
  | USE view_paths SEMI
  | FN IDENT maybe_generics fn_decl inner_attrs_and_block
  ;

fn_decl : /* loose */ LPAREN maybe_args RPAREN ret_ty ;
maybe_args : /* nothing */ | args ;
args : arg | arg COMMA args ;
arg : (arg_mode)? (MUT)? pat COLON ty ;
arg_mode : ANDAND | PLUS ;
ret_ty : RARROW NOT
  | RARROW ty
  | /* nothing */
  ;



// not treating _ specially... I don't think I have to.
pat : AT pat
  | TILDE pat
  | AND pat
  | LPAREN RPAREN
  | LPAREN pats RPAREN
  | /* loose */ bracketdelim // vectors
  | expr_res_no_bar_op (DOTDOT expr_res_no_bar_op)?
  | REF mutability pat_ident
  | COPY pat_ident
  | path
  | path AT pat
  | path MOD_SEP generics
  | path (MOD_SEP generics)? LBRACE pat_fields RBRACE
  | path (MOD_SEP generics)? LPAREN STAR RPAREN
  | path (MOD_SEP generics)? LPAREN maybe_pats RPAREN
  ;
// I may not be supporting goofy trailing-comma properly here...
maybe_pats : /* nothing */ | pats ;
pats : pat | pat COMMA pats ;

const_item : STATIC IDENT COLON ty EQ expr SEMI ;
enum_item : ENUM IDENT maybe_generics EQ ty SEMI
  | ENUM IDENT maybe_generics /* loose: */ bracedelim
  ;
// at most one common field declaration?
//enum_def : outer_at// loose:
//    bracedelim ;
view_paths : view_path | view_path COMMA view_paths ;
view_path : MOD? IDENT EQ non_global_path
  | MOD? non_global_path MOD_SEP LBRACE RBRACE
  | MOD? non_global_path MOD_SEP LBRACE ident_seq RBRACE
  | MOD? non_global_path MOD_SEP STAR
  | MOD? non_global_path
  ;

// UNIMPLEMENTED:
inner_attrs_and_block : STAR STAR ;
expr_res_no_bar_op : STAR STAR ;
mutability : STAR STAR ;
pat_ident : STAR STAR ;
pat_fields : STAR STAR ;

outer_attrs : /* nothing */ | outer_attr outer_attrs ;
outer_attr : POUND LBRACKET meta_item RBRACKET
  | OUTER_DOC_COMMENT ; 
inner_attr : POUND LBRACKET meta_item RBRACKET SEMI
  | INNER_DOC_COMMENT ;
meta_item : IDENT
  | IDENT EQ lit
  | IDENT LPAREN meta_item_seq RPAREN ;
meta_item_seq : | meta_item_nonempty_seq ;
meta_item_nonempty_seq : meta_item
  | meta_item COMMA meta_item_nonempty_seq ;



// from a parser standpoint, it would be simpler just
// to allow lifetimes and type params to be intermixed.
maybe_generics : /* nothing */ | generics ;
generics : LT GT
  | LT generics_list GT ;
generics_list : LIFETIME
  | LIFETIME COMMA generics_list
  | ty_params ;
ty_params : ty_param | ty_param COMMA ty_params ;
ty_param : IDENT | IDENT COLON | IDENT COLON boundseq ;
boundseq : bound | bound PLUS boundseq ;
bound : (AMP STATIC)? ty;

ident_seq : IDENT | IDENT COMMA ident_seq ;

path : MOD_SEP? non_global_path ;
non_global_path : IDENT (MOD_SEP IDENT)* ;



//UNIMPLEMENTED:
expr : STAR STAR ;
//mod_item : use | trait | struct | type | fundecl | impl | implfor ;

use : USE path SEMI
  | USE path MOD_SEP STAR SEMI
  | USE path MOD_SEP bracedelim SEMI ;

trait : PUB? TRAIT IDENT bracedelim;
struct : PUB? STRUCT IDENT bracedelim;
type : PUB? TYPE IDENT EQ ty SEMI;
implfor : PUB? IMPL IDENT FOR ty bracedelim ;
impl : PUB? IMPL ty bracedelim ;

lit : TRUE
  | FALSE
  | LIT_INT
  | LIT_FLOAT
  | LIT_STR
  | LPAREN RPAREN ;

ty : AT ty
  | path maybe_generics
  | FN parendelim RARROW ty
  | parendelim
  | TILDE ty
  | bracketdelim ;

// below here it looks pretty good.
tt : nondelim | delimited ;
delimited : parendelim
  | bracketdelim
  | bracedelim;
parendelim : LPAREN tt* RPAREN ;
bracketdelim : LBRACKET tt* RBRACKET ;
bracedelim : LBRACE tt* RBRACE ;
nondelim :
    // putting in keywords to simplify things:
    AS
  | ASSERT
  | BREAK
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
  | STATIC
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
  |  STAR
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
  |  OUTER_DOC_COMMENT
  |  INNER_DOC_COMMENT;

outer_doc_comment : OUTER_DOC_COMMENT ;
inner_doc_comment : INNER_DOC_COMMENT ;

// putting keywords in to simplify things:
AS : 'as' ;
ASSERT : 'assert' ;
BREAK : 'break' ;
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
STATIC : 'static' ;
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
STAR : '*' ;
BINOP : '<<' | '>>' 
      | [-&|+/^%] ;
BINOPEQ : BINOP '=' | '*=';
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
OUTER_DOC_COMMENT : '///' '/' * NON_SLASH_OR_WS ~[\n]*
  | '///' '/' * [ \r\n\t] ~[ \r\n\t] ~[\n]* 
    // again, we have to do a funny dance to fence out
    // only-stars.
    // CAN'T ABSTRACT OVER BLOCK_CHARS; learned this
    // the hard way ... :(
  | '/**' (~[*] | ('*'+ ~[*/]))* ~[*] (~[*] | ('*'+ ~[*/]))* '*'+ '/' ;
INNER_DOC_COMMENT : '//!' ~[\n]*
  | '/*!' (~[*] | ('*'+ ~[*/]))* '*'+ '/' ;

// HELPER DEFINITIONS:

WS : [ \t\r\n]+ -> skip ; // skip spaces, tabs, newlines
OTHER_LINE_COMMENT : '//' ~[\n] * -> skip ;
OTHER_BLOCK_COMMENT : '/*' (~[*] | ('*'+ ~[*/]))* '*'+ '/' -> skip ;


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

STRCHAR : ~[\\"] | '\\' STRESCAPE ; 
STRESCAPE : '\n' | ESCAPEDCHAR ;

IDSTART : [_a-zA-Z]  | XIDSTART ;
IDCONT : [_a-zA-Z0-9] | XIDCONT ;

NON_SLASH_OR_WS : ~[ \t\r\n/] ;
