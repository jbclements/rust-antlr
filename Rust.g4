grammar Rust;

// splitting issues: &&, <<, >>, >>=, ||
// be more consistent in use of *_list, *_seq, and *s.
// add parse_opt_abi_thingy
// NB: associativity may be wrong all over the place.
// re-check trailing comma legal locations

// UNICODE: okay, I just got lost in Unicode Standard Annex #31,
// "Unicode Identifier and Pattern Syntax".
// I get the sense that Rust intends to abide by Unicode standards
// for identifiers; what I've done in this file approximates this,
// I believe.

@lexer::members {
      static int dotChar = 46;
      public boolean followed_by_ident_or_dot() {

        CharStream cs = getInputStream();
        int nextChar = cs.LA(1);
        // KNOWN POTENTIAL ISSUE : this fn needs to be
        // aligned with the list appearing in xidstart....
        return (java.lang.Character.isUnicodeIdentifierStart(nextChar)
                || nextChar == dotChar);
      }

      public boolean at_beginning_of_file() {
        return (getInputStream().index() == 0);
      }

    }


import "xidstart" , "xidcont";

// parsing a whole file as a 'tts' should work on current sources.
tts : tt* ;

// parsing a whole file as a 'prog' will be spotty.
prog : module_contents ;
module_contents : inner_attr* extern_mod_view_item* view_item* mod_item*;

// MODULE ITEMS :
extern_mod_view_item : attrs_vis EXTERN MOD ident (LPAREN (meta_item_seq)? RPAREN)? SEMI ;
view_item : attrs_vis use ;
mod_item : attrs_vis items_with_visibility
  | outer_attrs impl_trait_for_type
  | macro_item ;
items_with_visibility : const_item
  | enum_decl
  | (UNSAFE)? item_fn_decl
  | impl
  | mod_decl
  | EXTERN (LIT_STR)? item_fn_decl
  | foreign_mod
  | struct_decl
  | type_decl
  | trait_decl
  ;
mod_decl : MOD ident SEMI
  | MOD ident LBRACE module_contents RBRACE ;
struct_decl : STRUCT ident (generic_decls)? LBRACE (struct_fields)? RBRACE 
  | STRUCT ident (generic_decls)? LPAREN (tys?) RPAREN SEMI
  | STRUCT ident (generic_decls)? SEMI ;
impl : IMPL (generic_decls)? ty impl_body ;
impl_trait_for_type : IMPL (generic_decls)? trait FOR ty impl_body ;
type_decl : TYPE ident (generic_decls)? EQ ty SEMI ;
enum_decl : ENUM ident (generic_decls)? EQ ty SEMI
  | ENUM ident (generic_decls)? LBRACE (enum_variant_decls)? RBRACE ;
enum_variant_decls : enum_variant_decl COMMA enum_variant_decls | enum_variant_decl (COMMA)? ;
enum_variant_decl : attrs_vis ident LBRACE (struct_fields)? RBRACE
  | attrs_vis ident LPAREN (tys)? RPAREN
  | attrs_vis ident EQ expr
  | attrs_vis ident
  ;
// trailing comma allowed:
struct_fields : struct_field COMMA struct_fields | struct_field (COMMA)? ;
struct_field
  : attrs_vis mutability ident COLON ty
  | outer_attrs DROP block
  ;
trait_decl: TRAIT ident (generic_decls)? (COLON trait_list)? LBRACE trait_method* RBRACE ;
trait_method
  : attrs_vis (UNSAFE)? FN ident (generic_decls)? fn_args_with_self ret_ty SEMI
  | attrs_vis (UNSAFE)? FN ident (generic_decls)? fn_args_with_self ret_ty fun_body;
fn_args_with_self : LPAREN (self_ty_and_args)? RPAREN;
self_ty_and_args
  : AND (lifetime)? mutability SELF (COMMA tylike_args)?
  | AT mutability SELF (COMMA tylike_args)?
  | TILDE mutability SELF (COMMA tylike_args)?
  | SELF (COMMA tylike_args)?
  | tylike_args
  ;
macro_item: path NOT (ident)? parendelim
  | path NOT (ident)? bracedelim ;
use : USE view_paths SEMI ;
item_fn_decl : FN ident (generic_decls)? LPAREN (args)? RPAREN ret_ty fun_body ;
foreign_mod :  EXTERN (LIT_STR)? MOD ident LBRACE inner_attr* foreign_item* RBRACE
  | EXTERN (LIT_STR)? LBRACE inner_attr* foreign_item* RBRACE ;
foreign_item
  : outer_attrs STATIC ident COLON ty SEMI
  | outer_attrs visibility (UNSAFE)? FN ident (generic_decls)? LPAREN (args)? RPAREN ret_ty SEMI
  ;


visibility : PUB | PRIV | /*nothing*/ ;
// overly loose on "const", but soon it will disappear completely?
mutability : MUT | CONST | /*nothing*/ ;

trait_list : trait | trait PLUS trait_list ;


impl_body : SEMI
  | /*loose*/ bracedelim ;

args : arg | args COMMA arg ;
arg : (arg_mode)? mutability pat COLON ty ;
arg_mode : AND AND | PLUS | obsoletemode ;
// obsolete ++ mode used in librustc/middle/region.rs
obsoletemode : PLUS PLUS ;

fn_block_args : fn_block_arg | fn_block_arg COMMA fn_block_args ;
fn_block_arg : (arg_mode)? mutability pat (COLON ty)? ;

attrs_vis : outer_attrs visibility ;


ret_ty : RARROW NOT
  | RARROW ty
  | /* nothing */
  ;
tylike_args : tylike_arg | tylike_args COMMA tylike_arg ;
tylike_arg : arg | ty ;

fun_body : LBRACE inner_attr* view_item* block_element* block_last_element? RBRACE ;
block : LBRACE view_item* block_element* block_last_element? RBRACE ;
block_element : expr_RL (SEMI)+
  | stmt_not_just_expr (SEMI)*
  ;
// the "stmt" production may simply consume an expr without its semicolon, so we need
// to refactor to split these apart, because in a block body we need the semicolon.
// if this is true of all callers, we could glue it back together again....
stmt : expr_RL | stmt_not_just_expr ;
// a statement that is not parsed by the expr_RL rule
stmt_not_just_expr : let_stmt
  | mac_expr
  | mod_item
  | expr_stmt
  ;

block_last_element : expr_RL | mac_expr | expr_stmt ;
mac_expr : ident NOT /*loose*/ parendelim ;

let_stmt : LET mutability local_var_decl (COMMA local_var_decl)* SEMI ;
local_var_decl : pat (COLON ty)? (EQ expr)? ;

// not treating '_' specially... I don't think I have to.
// "refutable" appears not to affect the parsing at this level.
pat : AT pat
  | TILDE pat
  | AND pat
  | LPAREN RPAREN
  | LPAREN pats RPAREN
  | /* loose */ bracketdelim // vectors
    // really, it's fairly weird to allow any expr here...
  | exprRB (DOTDOT exprRB)?
  | REF mutability ident
  | COPYTOK ident
  | path AT pat
  | path_with_colon_tps
  | path_with_colon_tps /*loose*/ bracedelim // LBRACE pat_fields RBRACE
  | path_with_colon_tps LPAREN STAR RPAREN
  | path_with_colon_tps LPAREN (pats)? RPAREN
  ;
pats : pat (COMMA)? | pat COMMA pats ;
pats_or : pat | pat OR pats_or ;

const_item : STATIC ident COLON ty EQ expr SEMI ;

// at most one common field declaration?
//enum_def : outer_at// loose:
//    bracedelim ;
view_paths : view_path | view_path COMMA view_paths ;
view_path : MOD? ident EQ non_global_path
  | MOD? non_global_path MOD_SEP LBRACE RBRACE
  | MOD? non_global_path MOD_SEP LBRACE ident_seq RBRACE
  | MOD? non_global_path MOD_SEP STAR
  | MOD? non_global_path
  ;


// UNIMPLEMENTED:
pat_fields : STAR STAR ;

outer_attrs : /* nothing */ | outer_attr outer_attrs ;
outer_attr : POUND LBRACKET meta_item RBRACKET
  | OUTER_DOC_COMMENT ; 
inner_attr : POUND LBRACKET meta_item RBRACKET SEMI
  | INNER_DOC_COMMENT ;
meta_item : ident
  | ident EQ lit
  | ident LPAREN (meta_item_seq)? RPAREN ;
meta_item_seq : meta_item
  | meta_item COMMA meta_item_seq ;



// from a parser standpoint, it would be simpler just
// to allow lifetimes and type params to be intermixed.
generic_decls : LT GT
  | LT generic_decls_list GT ;
generic_decls_list : lifetime
  | lifetime COMMA generic_decls_list
  | ty_params ;
ty_params : ty_param | ty_param COMMA ty_params ;
ty_param : ident | ident COLON | ident COLON boundseq ;
boundseq : bound | bound PLUS boundseq ;
bound : STATIC_LIFETIME | ty | obsoletekind ;
// take these out?
obsoletekind : COPYTOK | CONST ;


colon_generics : MOD_SEP generics ;
generics : LT GT
  | LT generics_list GT ;
generics_list : lifetime
  | lifetime COMMA generics_list
  | tys ;

lifetimes_in_braces : LT (lifetimes)? GT ;
lifetimes : lifetime | lifetimes COMMA lifetime ;

ident_seq : ident (COMMA)? | ident COMMA ident_seq ;

path : MOD_SEP? non_global_path ;
non_global_path : ident (MOD_SEP ident)* ;

// EXPRS

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
expr_6 : expr_6 OR OR expr_7
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
  | AND (lifetime)? mutability expr_prefix
  | AT mutability expr_prefix
  | TILDE expr_prefix
  | expr_dot_or_call
  ;
expr_dot_or_call
  : expr_dot_or_call DOT ident (MOD_SEP generics)? (/*loose*/parendelim)?
  | expr_dot_or_call /*loose*/parendelim
  | expr_dot_or_call /*loose*/bracketdelim
  | expr_bottom
  ;
expr_bottom : /*loose*/ parendelim
  | expr_lambda
  | expr_stmt
  | /*loose*/ bracketdelim
  | __LOG LPAREN expr COMMA expr RPAREN
  | LOOP (ident)?
  | RETURN expr
  | RETURN
  | BREAK (ident)?
  | COPYTOK expr
  | expr_macro_invocation
  | path_with_colon_tps /*loose*/ bracedelim 
  | path_with_colon_tps
  | lit
  ;

// once again, with stmt_on_lhs restriction . the token
// fooRL is just like foo but doesn't allow an expr_statement
// as its beginning.

// in the NO_LHS_STMT mode, we're concerned about
// statements like if true {3} else {4} |a|a, which we
// want to parse as a statement followed by an expression,
// rather than as (if... | a) | a.

expr_RL : expr_1RL EQ expr
  | expr_1RL BINOPEQ expr
  | expr_1RL DARROW expr
  | expr_1RL
  ;
expr_1RL : expr_1RL OR expr_2
  | expr_2RL ;
expr_2RL : expr_2RL AND expr_3
  | expr_3RL ;
expr_3RL : expr_3RL EQEQ expr_4
  | expr_3RL NE expr_4
  | expr_4RL ;
expr_4RL : expr_4RL LT expr_5
  | expr_4RL LE expr_5
  | expr_4RL GE expr_5
  | expr_4RL GT expr_5
  | expr_5RL
  ;
// there is no precedence 5 ...
expr_5RL : expr_6RL;
expr_6RL : expr_6RL OR OR expr_7
  | expr_7RL ;
expr_7RL : expr_7RL CARET expr_8
  | expr_8RL ;
expr_8RL : expr_8RL AND expr_9
  | expr_9RL ;
expr_9RL : expr_9RL LT LT expr_10
  | expr_9RL GT GT expr_10
  | expr_10RL ;
expr_10RL : expr_10RL PLUS expr_11
  | expr_10RL MINUS expr_11
  | expr_11RL ;
expr_11RL : expr_11RL AS ty
  | expr_12RL ;
expr_12RL : expr_12RL STAR expr_prefix
  | expr_12RL DIV expr_prefix
  | expr_12RL REM expr_prefix
  | expr_prefixRL ;
expr_prefixRL : NOT expr_prefix
  | MINUS expr_prefix
  | STAR expr_prefix
  | AND (lifetime)? mutability expr_prefix
  | AT mutability expr_prefix
  | TILDE expr_prefix
  | expr_dot_or_callRL
  ;
expr_dot_or_callRL
    // strange exception here: we allow .f() after stmt_exprs
  : expr_dot_or_call DOT ident (MOD_SEP generics)? (/*loose*/parendelim)?
  | expr_dot_or_callRL /*loose*/parendelim
  | expr_dot_or_callRL /*loose*/bracketdelim
  | expr_bottomRL
  ;
expr_bottomRL : /*loose*/ parendelim
  | expr_lambda
  // no expr_stmt here
  | /*loose*/ bracketdelim
  | __LOG LPAREN expr COMMA expr RPAREN
  | LOOP (ident)?
  | RETURN expr
  | RETURN
  | BREAK (ident)?
  | COPYTOK expr
    // this is an ambiguity, right?
  | expr_macro_invocation
  | path_with_colon_tps /*loose */ bracedelim //LBRACE field_exprs RBRACE
  | path_with_colon_tps
  | lit
  ;

// exprRB
// once again, with no | allowed.

exprRB : expr_2 EQ exprRB
  | expr_2 BINOPEQ exprRB
  | expr_2 DARROW exprRB
    // skipping over OR :
  | expr_2 
  ;

// expr_RBB
// once again, with no | or || allowed.

expr_RBB
    // skipping over OR:
  : expr_2RBB EQ expr_RBB
  | expr_2RBB BINOPEQ expr_RBB
  | expr_2RBB DARROW expr_RBB
  | expr_2RBB 
  ;
expr_2RBB : expr_2RBB AND expr_3RBB
  | expr_3RBB ;
expr_3RBB : expr_3RBB EQEQ expr_4RBB
  | expr_3RBB NE expr_4RBB
  | expr_4RBB ;
expr_4RBB : expr_4RBB LT expr_5RBB
  | expr_4RBB LE expr_5RBB
  | expr_4RBB GE expr_5RBB
  | expr_4RBB GT expr_5RBB
  | expr_5RBB
  ;
// there is no precedence 5 ...
// skipping over OR OR :
expr_5RBB : expr_7;


// things that can be either statements (without semicolons) or expressions
// these can't appear in certain positions, e.g. 'if true {3} else {4} + 19'
expr_stmt
  : expr_stmt_block
  | expr_stmt_not_block
  ;
expr_stmt_block : (UNSAFE)? block ;
expr_stmt_not_block
  : expr_if
  | expr_match
  | expr_while
  | expr_loop
  | expr_for
  | expr_do
  ;

expr_if : IF expr block (ELSE (block | expr_if))? ;
expr_for : FOR expr_RBB (OR (fn_block_args)? OR)? block;
expr_do : DO expr_RBB (OR (fn_block_args)? OR)? block;
expr_while : WHILE expr block ;
expr_loop
  : LOOP (UNSAFE)? block
  | LOOP ident COLON block
  ;
expr_match : MATCH expr LBRACE (match_clauses)? RBRACE ;
match_clauses : match_final_clause | match_clause match_clauses ;
match_final_clause : pats_or (IF expr)? FAT_ARROW (expr_RL | expr_stmt_not_block | expr_stmt_block ) (COMMA)? ;
match_clause : pats_or (IF expr)? FAT_ARROW (expr_RL COMMA | expr_stmt_not_block COMMA | expr_stmt_block (COMMA)? ) ;

expr_lambda : OR (fn_block_args)? OR expr ;

// SELF and STATIC may be used as identifiers
// not sure about underscore. should it even be a token?
ident : IDENT | SELF | STATIC | UNDERSCORE ;
lifetime : STATIC_LIFETIME | LIFETIME ;

field_exprs : field_expr field_trailer
  | field_expr COMMA field_exprs ;
field_trailer : DOTDOT expr
  | COMMA
  | /* nothing */
  ;
// unimplemented
field_expr : mutability ident COLON expr ;

expr_macro_invocation :
    path_with_tps NOT parendelim
  | path_with_tps NOT bracedelim ;

path_with_tps : path (generics)? ;
path_with_colon_tps : path (colon_generics)? ;

lit : TRUE
  | FALSE
  | LIT_INT
  | LIT_FLOAT
  | LIT_STR
  | LPAREN RPAREN ;

// trait : ty that gets parsed as a ty_path
// BUG IN PARSER: (A) parses as a trait.
trait : path (generics)? ;
ty : LPAREN RPAREN
  | LPAREN ty RPAREN
  | LPAREN ty COMMA RPAREN
  | LPAREN tys RPAREN
  | AT box_or_uniq_pointee
  | TILDE box_or_uniq_pointee
  | STAR mutability ty
  | path (generics)?
  | LBRACKET obsoleteconst ty COMMA DOTDOT expr RBRACKET
  | LBRACKET obsoleteconst ty RBRACKET
  | AND borrowed_pointee
    // something going in here re: ABI
  | EXTERN (LIT_STR)? (UNSAFE)? FN (lifetimes_in_braces)? LPAREN (tylike_args)? RPAREN ret_ty
  | ty_closure
  | path (generics)?
  ;
tys : ty | tys COMMA ty ;
box_or_uniq_pointee : (lifetime)? ty_closure
  | mutability ty ;
borrowed_pointee : (lifetime)? ty_closure
  | (lifetime)? mutability ty ;
ty_closure : (UNSAFE)? (ONCE)? FN (lifetimes_in_braces)? LPAREN (tylike_args)? RPAREN ret_ty ;

// obsolete:
obsoleteconst : (CONST)? ;

// TOKEN TREES:
tt : nondelim | delimited ;
delimited : parendelim
  | bracketdelim
  | bracedelim;
parendelim : LPAREN tt* RPAREN ;
bracketdelim : LBRACKET tt* RBRACKET ;
bracedelim : LBRACE tt* RBRACE ;
    // putting in keywords to simplify things:
nondelim
  : AS
  | BREAK
  | CONST
  | COPYTOK
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
  | SELF
    // I don't think super needs to be a keyword.
//  | SUPER
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
  |  STATIC_LIFETIME
  |  LIFETIME
  // For interpolation
  // |  INTERPOLATED
  |  OUTER_DOC_COMMENT
  |  INNER_DOC_COMMENT;

outer_doc_comment : OUTER_DOC_COMMENT ;
inner_doc_comment : INNER_DOC_COMMENT ;

// putting keywords in to simplify things:
AS : 'as' ;
BREAK : 'break' ;
CONST : 'const' ;
COPYTOK : 'copy' ;
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
SELF : 'self' ;
STATIC : 'static' ;
STRUCT : 'struct' ;
// I don't think super should be a keyword...
// SUPER : 'super' ;
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
          | [0-9] DECDIGIT* INTLIT_TY?
          ;

// we definitely need lookahead here...
LIT_FLOAT : [0-9] DECDIGIT* '.' {!followed_by_ident_or_dot()}?
          // nb: digit following '.' can't be underscore.
          | [0-9] DECDIGIT* '.' [0-9] DECDIGIT* LITFLOAT_EXP? LITFLOAT_TY?
          | [0-9] DECDIGIT* LITFLOAT_EXP LITFLOAT_TY?
          | [0-9] DECDIGIT* LITFLOAT_TY
          ;

LIT_STR : '\"' STRCHAR* '\"' ;
IDENT : IDSTART IDCONT* ;
UNDERSCORE : '_' ;


// there's potential ambiguity with char constants,
// but I think that the greedy read will do the "right
// thing"
STATIC_LIFETIME : '\'static' ;
LIFETIME : '\'' IDENT
    // should this be a lifetime? :
  | '\'' SELF
  ;
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
SHEBANG_LINE : {at_beginning_of_file()}? '#!' ~[\n]* '\n' -> skip ;

BINDIGIT : [0-1_] ;
DECDIGIT : [0-9_] ;
HEXDIGIT : [0-9a-fA-F_] ;
INTLIT_TY : ('u'|'i') ('8'|'16'|'32'|'64')? ;
LITFLOAT_EXP : [eE] [+-]? DECDIGIT+ ;
LITFLOAT_TY : 'f' ('32'|'64')? ;

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
