grammar Rust;

// splitting issues: &&, <<, >>, >>=, ||
// be more consistent in use of *_list, *_seq, and *s.
// NB: associativity may be wrong all over the place.
// re-check trailing comma legal locations

// UNICODE: okay, I just got lost in Unicode Standard Annex #31,
// "Unicode Identifier and Pattern Syntax".
// I get the sense that Rust intends to abide by Unicode standards
// for identifiers; what I've done in this file approximates this,
// I believe.

// handling of macros is not yet a match for the parser, in part
// because the parser does strange things; when there's a macro
// in statement position, it parses it as a statement even if
// it's an expression.

@lexer::members {
      static int dotChar = 46;

      // is this character followed by an identifier or
      // a dot? this is used in parsing numbers, to distinguish
      // floating-point numbers from ranges and method calls.
      public boolean followed_by_ident_or_dot() {
        CharStream cs = getInputStream();
        int nextChar = cs.LA(1);
        // KNOWN POTENTIAL ISSUE : this fn needs to be
        // aligned with the list appearing in xidstart....
        return (java.lang.Character.isUnicodeIdentifierStart(nextChar)
                || nextChar == dotChar);
      }

      // are we at the beginning of the file? This is needed in
      // order to parse shebangs.
      public boolean at_beginning_of_file() {
        return (getInputStream().index() == 0);
      }

    }

import "xidstart" , "xidcont";

// you can either treat a program as a sequence of token-trees--this is
// the "s-expression" approach to parsing--or as a prog.

// a sequence of token trees
tts : tt* ;

// module contents
prog : module_contents ;
module_contents : inner_attr* extern_mod_view_item* view_item* mod_item*;

// MODULE ITEMS :
extern_mod_view_item : attrs_and_vis EXTERN MOD ident (lib_selectors)? SEMI ;
view_item : attrs_and_vis USE view_paths SEMI ;
mod_item
  : attrs_and_vis mod_decl
  | attrs_and_vis foreign_mod
  | attrs_and_vis type_decl
  | attrs_and_vis struct_decl
  | attrs_and_vis enum_decl
  | attrs_and_vis trait_decl
  | attrs_and_vis const_item
  | attrs_and_vis impl
  | outer_attrs impl_trait_for_type
  | attrs_and_vis (UNSAFE)? item_fn_decl
  | attrs_and_vis EXTERN (LIT_STR)? item_fn_decl
  | macro_item
  ;

mod_decl
  : MOD ident SEMI
  | MOD ident LBRACE module_contents RBRACE
  ;

foreign_mod
  : EXTERN (LIT_STR)? MOD ident LBRACE inner_attr* foreign_item* RBRACE
  | EXTERN (LIT_STR)? LBRACE inner_attr* foreign_item* RBRACE ;
foreign_item
  : outer_attrs STATIC ident COLON ty SEMI
  | outer_attrs visibility (UNSAFE)? FN ident (LT (generic_decls)? GT)? LPAREN (args)? RPAREN ret_ty SEMI
  ;

type_decl : TYPE ident (LT (generic_decls)? GT)? EQ ty SEMI ;

struct_decl
  : STRUCT ident (LT (generic_decls)? GT)? LBRACE (struct_fields (COMMA)?)? RBRACE
  | STRUCT ident (LT (generic_decls)? GT)? LPAREN (tys)? RPAREN SEMI
  | STRUCT ident (LT (generic_decls)? GT)? SEMI
  ;
struct_fields : struct_field COMMA struct_fields | struct_field ;
struct_field
  : attrs_and_vis mutability ident COLON ty
  | outer_attrs DROP block
  ;

enum_decl
  : ENUM ident (LT (generic_decls)? GT)? LBRACE (enum_variant_decls (COMMA)?)? RBRACE
  ;
enum_variant_decls : enum_variant_decl COMMA enum_variant_decls | enum_variant_decl ;
enum_variant_decl
  : attrs_and_vis ident LBRACE (struct_fields (COMMA)?)? RBRACE
  | attrs_and_vis ident LPAREN (tys)? RPAREN
  | attrs_and_vis ident EQ expr
  | attrs_and_vis ident
  ;

trait_decl: TRAIT ident (LT (generic_decls)? GT)? (COLON traits)? LBRACE trait_method* RBRACE ;
trait_method
  : attrs_and_vis (UNSAFE)? FN ident (LT (generic_decls)? GT)? LPAREN (self_ty_and_maybenamed_args)? RPAREN ret_ty SEMI
  | attrs_and_vis (UNSAFE)? FN ident (LT (generic_decls)? GT)? LPAREN (self_ty_and_maybenamed_args)? RPAREN ret_ty fun_body
  ;

impl : IMPL (LT (generic_decls)? GT)? ty impl_body ;
impl_trait_for_type : IMPL (LT (generic_decls)? GT)? trait FOR ty impl_body ;
impl_body : SEMI
  | LBRACE impl_method* RBRACE ;
impl_method : attrs_and_vis (UNSAFE)? FN ident (LT (generic_decls)? GT)? LPAREN (self_ty_and_args)? RPAREN ret_ty fun_body  ;

item_fn_decl : FN ident (LT (generic_decls)? GT)? LPAREN (args)? RPAREN ret_ty fun_body ;
fun_body : LBRACE inner_attr* view_item* block_element* (block_last_element)? RBRACE ;
block : LBRACE view_item* block_element* (block_last_element)? RBRACE ;
block_element : expr_RL (SEMI)+
  | stmt_not_just_expr (SEMI)*
  ;
block_last_element : expr_RL | macro_parens | expr_stmt ;
ret_ty : RARROW NOT
  | RARROW ty
  | /* nothing */
  ;

macro_item
  : ident NOT (ident)? parendelim
  | ident NOT (ident)? bracedelim
  ;

attrs_and_vis : outer_attrs visibility ;
visibility : PUB | PRIV | /*nothing*/ ;
// overly loose on "const", but soon it will disappear completely?
mutability : MUT | CONST | /*nothing*/ ;
lib_selectors : LPAREN (meta_items)? RPAREN ;
outer_attrs : /* nothing */ | outer_attr outer_attrs ;
outer_attr : POUND LBRACKET meta_item RBRACKET
  | OUTER_DOC_COMMENT ;
inner_attr : POUND LBRACKET meta_item RBRACKET SEMI
  | INNER_DOC_COMMENT ;
meta_item : ident
  | ident EQ lit
  | ident LPAREN (meta_items)? RPAREN ;
meta_items : meta_item
  | meta_item COMMA meta_items ;



args : arg | arg COMMA args ;
arg : (arg_mode)? mutability pat COLON ty ;
arg_mode : AND AND | PLUS | obsoletemode ;
// obsolete ++ mode used in librustc/middle/region.rs
obsoletemode : PLUS PLUS ;

self_ty_and_args
  : self_ty (COMMA args)?
  | args
  ;
self_ty_and_maybenamed_args
  : self_ty (COMMA maybenamed_args)?
  | maybenamed_args
  ;
self_ty
  : AND (lifetime)? mutability SELF
  | AT mutability SELF
  | TILDE mutability SELF
  | SELF
  ;

maybetyped_args : maybetyped_arg | maybetyped_arg COMMA maybetyped_args ;
maybetyped_arg : (arg_mode)? mutability pat (COLON ty)? ;
maybenamed_args : maybenamed_arg | maybenamed_arg COMMA maybenamed_args ;
maybenamed_arg : arg | ty ;


// not treating '_' specially... I don't think I have to.
pat : AT pat
  | TILDE pat
  | AND pat
  | LPAREN RPAREN
  | LPAREN pats RPAREN
  | LBRACKET (vec_pats)? RBRACKET
    // definitely ambiguity here with ident patterns
  | expr_RB (DOTDOT expr_RB)?
  | REF mutability ident
  | COPYTOK ident
  | path AT pat
  | path_with_colon_tps
  | path_with_colon_tps LBRACE pat_fields RBRACE
  | path_with_colon_tps LPAREN STAR RPAREN
  | path_with_colon_tps LPAREN (pats)? RPAREN
  ;
pats : pat (COMMA)? | pat COMMA pats ;
pats_or : pat | pat OR pats_or ;
vec_pats : pat
  | DOTDOT ident
  | pat COMMA vec_pats
  | DOTDOT ident COMMA vec_pats_no_slice ;
vec_pats_no_slice : pat | pat COMMA vec_pats_no_slice ;
const_item : STATIC ident COLON ty EQ expr SEMI ;

view_paths : view_path | view_path COMMA view_paths ;
view_path : (MOD)? ident EQ non_global_path
  | (MOD)? non_global_path MOD_SEP LBRACE RBRACE
  | (MOD)? non_global_path MOD_SEP LBRACE idents (COMMA)? RBRACE
  | (MOD)? non_global_path MOD_SEP STAR
  | (MOD)? non_global_path
  ;

pat_fields
  : IDENT (COLON pat)?
  | IDENT (COLON pat)? COMMA pat_fields
  | UNDERSCORE
  ;


traits : trait | trait PLUS traits ;
trait : path (LT (generics)? GT)? ;
tys : ty | ty COMMA tys ;
ty : LPAREN RPAREN
  | LPAREN ty RPAREN
  | LPAREN ty COMMA RPAREN
  | LPAREN tys RPAREN
  | AT box_or_uniq_pointee
  | TILDE box_or_uniq_pointee
  | STAR mutability ty
  | path (LT (generics)? GT)?
  | LBRACKET (obsoleteconst)? ty COMMA DOTDOT expr RBRACKET
  | LBRACKET (obsoleteconst)? ty RBRACKET
  | AND borrowed_pointee
  | EXTERN (LIT_STR)? (UNSAFE)? ty_fn
  | ty_closure
  | path (LT (generics)? GT)?
  ;
box_or_uniq_pointee
  : (lifetime)? ty_closure
  | mutability ty ;
borrowed_pointee
  : (lifetime)? ty_closure
  | (lifetime)? mutability ty ;
ty_closure
  : (UNSAFE)? (ONCE)? ty_fn
  ;
ty_fn : FN (LT (lifetimes)? GT)? LPAREN (maybenamed_args)? RPAREN ret_ty ;

// obsolete:
obsoleteconst : CONST ;



// because of the bifurcated treatment of statements and semicolon requirements,
// there's no "stmt" production; instead, upstream uses are treated independently.

// a statement that is not parsed by the expr_RL rule
stmt_not_just_expr
  : let_stmt
    // this one requires parens. I think this may be accidental,
    // because mod_item includes both kinds of macro invocation... and
    // this one can fall through to that one.
  | macro_parens
  | mod_item
  | expr_stmt
  ;
let_stmt : LET mutability local_var_decl (COMMA local_var_decl)* SEMI ;
local_var_decl : pat (COLON ty)? (EQ expr)? ;



// from a parser standpoint, it would be simpler just
// to allow lifetimes and type params to be intermixed.
generic_decls
  : lifetime
  | lifetime COMMA generic_decls
  | ty_params ;
ty_params : ty_param | ty_param COMMA ty_params ;
ty_param : ident | ident COLON | ident COLON boundseq ;
boundseq : bound | bound PLUS boundseq ;
bound : STATIC_LIFETIME | ty | obsoletekind ;
// take these out?
obsoletekind : COPYTOK | CONST ;


generics : lifetime
  | lifetime COMMA generics
  | tys ;

lifetimes_in_braces : LT (lifetimes)? GT ;
lifetimes : lifetime | lifetime COMMA lifetimes ;

idents : ident | ident COMMA idents ;

path : MOD_SEP? non_global_path ;
non_global_path : ident (MOD_SEP ident)* ;

// EXPRS

exprs : expr | expr COMMA exprs ;
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
// refactoning to eliminate left-recursion would make this less readable...
expr_dot_or_call
  : expr_dot_or_call DOT ident (MOD_SEP LT (generics)? GT)? (LPAREN (exprs)? RPAREN)?
  | expr_dot_or_call LPAREN (exprs)? RPAREN
  | expr_dot_or_call LBRACKET expr RBRACKET
  | expr_bottom
  ;
expr_bottom
// this covers (), (e), and tuples, including our goofy one-tuple:
  : LPAREN (exprs (COMMA)?)? RPAREN
  | expr_lambda
  | expr_stmt
  | expr_vector
  | __LOG LPAREN expr COMMA expr RPAREN
  | LOOP (ident)?
  | RETURN expr
  | RETURN
  | BREAK (ident)?
  | COPYTOK expr
    // this will overlap with the whole-stmt macro-invocation rule...
    // I don't think I can bear to
  | macro
  | path_with_colon_tps LBRACE field_inits (COMMA DOTDOT expr | (COMMA)?) RBRACE
  | path_with_colon_tps
  | lit
  ;

// once again, with stmt_on_lhs restriction . the token
// fooRL is just like foo but doesn't allow an expr_statement
// as its beginning.

// in this mode, we're concerned about
// statements like if true {3} else {4} |a|a, which we
// want to parse as a statement followed by a closure expression,
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
  : expr_dot_or_call DOT ident (MOD_SEP LT (generics)? GT)? (LPAREN (exprs)? RPAREN)?
  | expr_dot_or_callRL LPAREN (exprs)? RPAREN
  | expr_dot_or_callRL LBRACKET expr RBRACKET
  | expr_bottomRL
  ;
expr_bottomRL
  : LPAREN (exprs (COMMA)?)? RPAREN
  | expr_lambda
  // no expr_stmt here
  | expr_vector
  | __LOG LPAREN expr COMMA expr RPAREN
  | LOOP (ident)?
  | RETURN expr
  | RETURN
  | BREAK (ident)?
  | COPYTOK expr
    // this is an ambiguity, right?
  | macro
  | path_with_colon_tps LBRACE field_inits (COMMA DOTDOT expr | (COMMA)?) RBRACE
  | path_with_colon_tps
  | lit
  ;

// expr_RB
// these exprs are restricted; they may not contain the "or" operator.
// this is used to artificially lower the precedence of | in patterns.

expr_RB : expr_2 EQ expr_RB
  | expr_2 BINOPEQ expr_RB
  | expr_2 DARROW expr_RB
    // skipping over OR :
  | expr_2 
  ;

// expr_RBB
// these exprs are restricted; they may not contain "or" or the
// double "oror" operator. This is used to prevent parsing of
// lambda terms following uses of "do" or "for" from being treated
// as "or" operators instead.

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
// blocks need special treatment because they don't require commas when used
// as the RHS of match clauses.
expr_stmt
  : expr_stmt_block
  | expr_stmt_not_block
  ;
expr_stmt_block : (UNSAFE)? block;
expr_stmt_not_block
  : expr_if
  | expr_match
  | expr_while
  | expr_loop
  | expr_for
  | expr_do
  ;

field_inits : field_init | field_init COMMA field_inits ;
field_init : mutability ident COLON expr ;
expr_vector : LBRACKET RBRACKET
  | LBRACKET expr (COMMA DOTDOT expr)? RBRACKET
  | LBRACKET expr COMMA exprs (COMMA)? RBRACKET ;
expr_if : IF expr block (ELSE (block | expr_if))? ;
expr_for : FOR expr_RBB (OR (maybetyped_args)? OR)? block;
expr_do : DO expr_RBB (OR (maybetyped_args)? OR)? block;
expr_while : WHILE expr block ;
expr_loop
  : LOOP (UNSAFE)? block
  | LOOP ident COLON block
  ;
expr_match : MATCH expr LBRACE (match_clauses)? RBRACE ;
match_clauses : match_final_clause | match_clause match_clauses ;
match_final_clause
  : pats_or (IF expr)? FAT_ARROW (expr_RL | expr_stmt_not_block | expr_stmt_block ) (COMMA)? ;
match_clause
  : pats_or (IF expr)? FAT_ARROW (expr_RL COMMA | expr_stmt_not_block COMMA | expr_stmt_block (COMMA)? ) ;

expr_lambda : OR (maybetyped_args)? OR expr ;

// SELF and STATIC may be used as identifiers
// not sure about underscore. should it even be a token?
ident : IDENT | SELF | STATIC | UNDERSCORE ;
lifetime : STATIC_LIFETIME | LIFETIME ;

macro
  : macro_parens
  | macro_braces
  ;
macro_parens : ident NOT parendelim ;
macro_braces : ident NOT bracedelim ;

path_with_tps : path (LT (generics)? GT)? ;
path_with_colon_tps : path (MOD_SEP LT (generics)? GT )? ;

lit
  : TRUE
  | FALSE
  | LIT_INT
  | LIT_FLOAT
  | LIT_STR
  | LPAREN RPAREN
  ;

// TOKEN TREES:
tt : nondelim | delimited ;
delimited
  : parendelim
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
  // adding AND and PLUS and MINUS (etc) to make the grammar tractable:
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
  // It's not necessary to distinguish these for parsing:
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
  // Interpolation isn't present until after expansion
  // |  INTERPOLATED
  |  OUTER_DOC_COMMENT
  |  INNER_DOC_COMMENT;

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

LIT_INT
  : LIT_CHAR
  | '0x' HEXDIGIT+ INTLIT_TY?
  | '0b' BINDIGIT+ INTLIT_TY?
  | [0-9] DECDIGIT* INTLIT_TY?
  ;

LIT_FLOAT
  : [0-9] DECDIGIT* '.' {!followed_by_ident_or_dot()}?
    // nb: digit following '.' can't be underscore.
  | [0-9] DECDIGIT* '.' [0-9] DECDIGIT* LITFLOAT_EXP? LITFLOAT_TY?
  | [0-9] DECDIGIT* LITFLOAT_EXP LITFLOAT_TY?
  | [0-9] DECDIGIT* LITFLOAT_TY
  ;

LIT_STR : '\"' STRCHAR* '\"' ;
IDENT : IDSTART IDCONT* ;
UNDERSCORE : '_' ;


STATIC_LIFETIME : '\'static' ;
LIFETIME
  : '\'' IDENT
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
