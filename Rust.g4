grammar Rust;

// splitting issues: &&, <<, >>, >>=, ||
// be more consistent in use of *_list, *_seq, and *s.
// worry about restrictions, incl. "expr-is-complete"

import "xidstart" , "xidcont";

// parsing a whole file as a 'tts' should work on current sources.
tts : tt* ;

// parsing a whole file as a 'prog' will not yet work; 
prog : inner_attr* mod_item*;

// maybe incomplete! :
mod_item : outer_attrs visibility items_with_visibility
  | outer_attrs impl_trait_for_type
  | macro_item ;
items_with_visibility : const_item
  | enum_decl
  | use
  | (UNSAFE)? item_fn_decl 
  | impl
  | mod_decl
  | EXTERN item_fn_decl
  | EXTERN item_foreign_mod
  | EXTERN MOD IDENT maybe_meta_item_seq SEMI
  | struct_decl
  | type_decl
  | trait_decl 
  ;
mod_decl : MOD IDENT SEMI
  | MOD IDENT /*loose*/ bracedelim ;
struct_decl : STRUCT IDENT maybe_generic_decls /*loose*/ bracedelim
  | STRUCT IDENT maybe_generic_decls /*loose*/ parendelim
  | STRUCT IDENT maybe_generic_decls SEMI ;
impl : IMPL maybe_generic_decls ty impl_body ;
impl_trait_for_type : IMPL maybe_generic_decls trait FOR ty impl_body ;
type_decl : TYPE IDENT maybe_generic_decls EQ ty SEMI ;
enum_decl : ENUM IDENT maybe_generic_decls EQ ty SEMI
  | ENUM IDENT maybe_generic_decls /* loose: */ bracedelim ;
trait_decl: TRAIT IDENT maybe_generic_decls (COLON trait_list)? /*loose*/ bracedelim ;
macro_item: path NOT (IDENT)? parendelim
  | path NOT (IDENT)? bracedelim ;
use : USE view_paths SEMI ;
item_fn_decl : FN IDENT maybe_generic_decls LPAREN maybe_args RPAREN ret_ty inner_attrs_and_block ;
item_foreign_mod :  EXTERN (LIT_STR)? MOD IDENT /*loose*/ bracedelim
  | EXTERN (LIT_STR)? /*loose*/bracedelim ;


visibility : (PUB | PRIV)? ;

trait_list : trait | trait PLUS trait_list ;

    
impl_body : SEMI
  | /*loose*/ bracedelim ;

maybe_args : /* nothing */ | args ;
args : arg | arg COMMA args ;
arg : (arg_mode)? (MUT)? pat COLON ty ;
arg_mode : AND AND | PLUS ;
ret_ty : RARROW NOT
  | RARROW ty
  | /* nothing */
  ;
maybe_tylike_args : /*nothing*/ | tylike_args ;
tylike_args : tylike_arg | tylike_arg COMMA tylike_args ;
tylike_arg : arg | ty ;

inner_attrs_and_block : /*loose*/ LBRACE inner_attr* tt* RBRACE ;

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
maybe_pats : /* nothing */ | pats ;
pats : pat (COMMA)? | pat COMMA pats ;

const_item : STATIC IDENT COLON ty EQ expr SEMI ;

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

mutability : MUT | CONST | /*nothing*/ ;

// UNIMPLEMENTED:
expr_res_no_bar_op : STAR STAR ;
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
maybe_meta_item_seq : /*nothing*/ | LPAREN meta_item_seq RPAREN;



// from a parser standpoint, it would be simpler just
// to allow lifetimes and type params to be intermixed.
maybe_generic_decls : /* nothing */ | generic_decls ;
generic_decls : LT GT
  | LT generic_decls_list GT ;
generic_decls_list : LIFETIME
  | LIFETIME COMMA generic_decls_list
  | ty_params ;
ty_params : ty_param | ty_param COMMA ty_params ;
ty_param : IDENT | IDENT COLON | IDENT COLON boundseq ;
boundseq : bound | bound PLUS boundseq ;
bound : (AND STATIC)? ty;

maybe_generics : /* nothing */ | generics ;
generics : LT GT
  | LT generics_list GT ;
generics_list : LIFETIME
  | LIFETIME COMMA generics_list
  | ty_seq ;

maybe_lifetimes : /*nothing*/ | lifetimes ;
lifetimes : LIFETIME | LIFETIME COMMA lifetimes ;

ident_seq : IDENT | IDENT COMMA ident_seq ;

path : MOD_SEP? non_global_path ;
non_global_path : IDENT (MOD_SEP IDENT)* ;



//UNIMPLEMENTED:
expr : expr_1 EQ expr
  | expr_1 BINOPEQ expr
  | expr_1 DARROW expr
  | expr_1
  ;
expr_1 : expr_1 OR expr_2
  | expr_2 ;
expr_2 : expr_2 AND expr_3
  | expr_3 ;
expr_3 : expr_3 EQEQ expr_4
  | expr_3 NE expr_4
  | expr_4 ;
expr_4 : expr_4 LT expr_5
  | expr_4 LE expr_5
  | expr_4 GE expr_5
  | expr_4 GT expr_5
  | expr_5
  ;
// there is no precedence 5 ...
expr_5 : expr_6;
expr_6 : expr_6 OR expr_7
  | expr_7 ;
expr_7 : expr_7 CARET expr_8
  | expr_8 ;
expr_8 : expr_8 AND expr_9
  | expr_9 ;
expr_9 : expr_9 LT LT expr_10
  | expr_9 GT GT expr_10
  | expr_10 ;
expr_10 : expr_10 PLUS expr_11
  | expr_10 MINUS expr_11
  | expr_11 ;
expr_11 : expr_11 AS ty
  | expr_12 ;
expr_12 : expr_12 STAR expr_prefix
  | expr_12 DIV expr_prefix
  | expr_12 REM expr_prefix
  | expr_prefix ;
expr_prefix : NOT expr_prefix
  | MINUS expr_prefix
  | STAR expr_prefix
  | AND (LIFETIME)? mutability expr_prefix
  | AT (MUT)? expr_prefix
  | TILDE expr_prefix
  | expr_dot_or_call
  ;
expr_dot_or_call : expr_bottom expr_dot_or_call_suffix ;
expr_bottom : /*loose*/ parendelim
  | /*loose*/ bracedelim
    /* unimplemented
  | expr_lambda
  | expr_if
  | expr_for
  | expr_do
  | expr_while
  | expr_loop
  | expr_match
  | expr_unsafe_block */
  | /*loose*/ bracketdelim
  | __LOG LPAREN expr COMMA expr RPAREN
  | RETURN expr
  | BREAK (IDENT)?
  | COPY expr
  | expr_macro_invocation
  | path_with_tps LBRACE field_exprs RBRACE
  | path_with_tps
  | lit
  ;
expr_dot_or_call_suffix : DOT IDENT (MOD_SEP generics)? (/*loose*/parendelim)? expr_dot_or_call_suffix
  | /*loose*/parendelim expr_dot_or_call_suffix
  | /*loose*/bracketdelim expr_dot_or_call_suffix
  | /* nothing */
  ;


field_exprs : field_expr field_trailer
  | field_expr COMMA field_exprs ;
field_trailer : DOTDOT expr
  | COMMA
  | /* nothing */
  ;
// unimplemented
field_expr : STAR STAR ;

expr_macro_invocation :
    path_with_tps NOT parendelim
  | path_with_tps NOT bracedelim ;

path_with_tps : path maybe_generics ;

lit : TRUE
  | FALSE
  | LIT_INT
  | LIT_FLOAT
  | LIT_STR
  | LPAREN RPAREN ;

// trait : ty that gets parsed as a ty_path
// BUG IN PARSER: (A) parses as a trait.
trait : path maybe_generics ;
ty : LPAREN RPAREN
  | LPAREN ty RPAREN
  | LPAREN ty COMMA RPAREN
  | LPAREN ty_seq RPAREN
  | AT box_or_uniq_pointee
  | TILDE box_or_uniq_pointee
  | STAR mutability ty
  | path maybe_generics
  | LBRACKET ty COMMA DOTDOT expr RBRACKET
  | LBRACKET ty RBRACKET
  | AND borrowed_pointee
    // something going in here re: ABI
  | EXTERN (UNSAFE)? FN maybe_lifetimes LPAREN maybe_tylike_args RPAREN ret_ty
  | ty_closure
  | path maybe_generics
  ;
ty_seq : ty | ty COMMA ty_seq ;
box_or_uniq_pointee : (LIFETIME)? ty_closure
  | mutability ty ;
borrowed_pointee : (LIFETIME)? ty_closure
  | (LIFETIME)? mutability ty ;
ty_closure : (UNSAFE)? (ONCE)? FN maybe_lifetimes LPAREN maybe_tylike_args RPAREN ret_ty ;


// TOKEN TREES:
tt : nondelim | delimited ;
delimited : parendelim
  | bracketdelim
  | bracedelim;
parendelim : LPAREN tt* RPAREN ;
bracketdelim : LBRACKET tt* RBRACKET ;
bracedelim : LBRACE tt* RBRACE ;
    // putting in keywords to simplify things:
nondelim : AS
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
  // adding AND and PLUS and MINUS to make the grammar tractable:
  |  AND
  |  PLUS
  |  MINUS
  |  DIV
  |  REM
  |  CARET
  |  OR
  |  EQ
  |  LT
  |  LE
  |  EQEQ
  |  NE
  |  GE
  |  GT
  |  NOT
  |  TILDE
  |  STAR
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
STATIC : 'static' ;
STRUCT : 'struct' ;
SUPER : 'super' ;
TRUE : 'true' ;
TRAIT : 'trait' ;
TYPE : 'type' ;
UNSAFE : 'unsafe' ;
USE : 'use' ;
WHILE : 'while' ;

PLUS   : '+' ;
AND    : '&' ;
MINUS  : '-' ;
DIV    : '/' ;
REM    : '%' ;
CARET  : '^' ;
OR     : '|' ;
EQ     : '=' ;
LE     : '<=' ;
LT     : '<';
EQEQ : '==' ;
NE   : '!=' ;
GE   : '>=' ;
GT   : '>' ;
NOT   : '!' ;
TILDE : '~' ;
STAR : '*' ;
BINOPEQ : '/=' | '%=' | '^=' | '|=' | '-=' |'*=' | '&=' | '+=' | '<<=' | '>>=' ;
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

BINDIGIT : [0-1_] ;
DECDIGIT : [0-9_] ;
HEXDIGIT : [0-9a-fA-F_] ;
INTLIT_TY : ('u'|'i') ('8'|'16'|'32'|'64')? ;
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
