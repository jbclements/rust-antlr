#lang racket

(require rackunit
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide lex-file)

;; lex a file into tokens, discard them
(define (lex-file file)
  (display (~a file "\n"))
  (define ip (open-input-file file))
  (port-count-lines! ip)
  (let loop ()
    (define next-token (rust-lexer ip))
    (cond [(eq? (position-token-token next-token) 'EOF)
           (begin (close-input-port ip)
                  #t)]
          [else (loop)])))

(define (lex-string string)
  (define ip (open-input-string string))
  (let loop ()
    (define next-token (rust-lexer ip))
    (cond [(eq? (position-token-token next-token) 'EOF)
           empty]
          [else (cons next-token (loop))])))

(define-empty-tokens empty-toks
  (AS BREAK CONST COPYTOK DO DROP ELSE ENUM EXTERN FALSE FN FOR IF
      IMPL LET __LOG LOOP MATCH MOD MUT ONCE PRIV PUB PURE REF 
      RETURN SELF STATIC STRUCT TRUE TRAIT TYPE UNSAFE USE WHILE
      PLUS AND MINUS DIV REM CARET OR EQ LE LT EQEQ NE GE GT NOT 
      TILDE STAR AT DOT DOTDOT COMMA SEMI COLON MOD_SEP
      RARROW LARROW DARROW FAT_ARROW LPAREN RPAREN LBRACKET 
      RBRACKET LBRACE RBRACE POUND DOLLAR))
(define-tokens data 
  (LIT_INT LIT_FLOAT LIT_STR IDENT UNDERSCORE STATIC_LIFETIME 
           LIFETIME OUTER_DOC_COMMENT INNER_DOC_COMMENT
           BINOPEQ))

(define rust-lexer
  (lexer-src-pos
   [(:or one-line-outer-doc-comment block-outer-doc-comment)
    (token-OUTER_DOC_COMMENT lexeme)]
   [(:or one-line-inner-doc-comment block-inner-doc-comment)
    (token-INNER_DOC_COMMENT lexeme)]
   
   ;; Skip comments, without accumulating extra position information
   [(:or rust-whitespace 
         one-line-comment
         block-comment) 
    (return-without-pos (rust-lexer input-port))]
   
   
   ["as" 'AS ]
   [ "break" 'BREAK ]
   [ "const" 'CONST ]
   [ "copy" 'COPYTOK ]
   [ "do" 'DO ]
   [ "drop" 'DROP ]
   [ "else" 'ELSE ]
   [ "enum" 'ENUM ]
   [ "extern" 'EXTERN ]
   [ "false"  'FALSE ]
   [ "fn" 'FN ]
   [ "for" 'FOR ]
   [ "if" 'IF ]
   [ "impl"   'IMPL ]
   [ "let" 'LET ]
   [ "__log" '__LOG ]
   [ "loop" 'LOOP ]
   [ "match" 'MATCH ]
   [ "mod" 'MOD ]
   [ "mut" 'MUT ]
   [ "once" 'ONCE ]
   [ "priv" 'PRIV ]
   [ "pub" 'PUB ]
   [ "pure" 'PURE ]
   [ "ref" 'REF ]
   [ "return" 'RETURN ]
   [ "self" 'SELF ]
   [ "static" 'STATIC ]
   [ "struct" 'STRUCT ]
   [ "true" 'TRUE ]
   [ "trait" 'TRAIT ]
   [ "type" 'TYPE ]
   [ "unsafe" 'UNSAFE ]
   [ "use" 'USE ]
   [ "while" 'WHILE ]
   [ "+" 'PLUS ]
   [ "&" 'AND ]
   [ "-" 'MINUS ]
   [ "/" 'DIV ]
   [ "%" 'REM ]
   [ "^" 'CARET ]
   [ "|" 'OR ]
   [ "=" 'EQ ]
   [ "<=" 'LE ]
   [ "<"'LT ]
   [ "==" 'EQEQ ]
   [ "!=" 'NE ]
   [ ">=" 'GE ]
   [ ">" 'GT ]
   [ "!" 'NOT ]
   [ "~" 'TILDE ]
   [ "*" 'STAR ]
   [ "@" 'AT ]
   [ "." 'DOT ]
   [ ".." 'DOTDOT ]
   [ "," 'COMMA ]
   [ ";" ' SEMI ]
   [ ":" 'COLON ]
   [ "->" 'RARROW ]
   [ "::" 'MOD_SEP ]
   [ "<-" 'LARROW ]
   [ "<->" 'DARROW ]
   [ "(" 'LPAREN ]
   [ "=>" 'FAT_ARROW ]
   [ ")" 'RPAREN ]
   [ "[" 'LBRACKET ]
   [ "]" 'RBRACKET ]
   [ "{" 'LBRACE ]
   [ "}" 'RBRACE ]
   [ "#" 'POUND ]
   [ "$" 'DOLLAR ]
   
   [(:: id-start (:* id-cont)) (token-IDENT lexeme)]
   [(:: (:/ #\0 #\9) (:* dec-digit) (:? intlit-ty)) 
    (token-LIT_INT lexeme)]
   [lit-char (token-LIT_INT lexeme)]
#|   LIT_INT
  : LIT_CHAR
  | '0x' HEXDIGIT+ INTLIT_TY?
  | '0b' BINDIGIT+ INTLIT_TY?
  | [0-9] DECDIGIT* INTLIT_TY?
  ;|#

   #|
LIT_FLOAT
  : [0-9] DECDIGIT* '.' {!followed_by_ident_or_dot()}?
    // nb: digit following '.' can't be underscore.
  | [0-9] DECDIGIT* '.' [0-9] DECDIGIT* LITFLOAT_EXP? LITFLOAT_TY?
  | [0-9] DECDIGIT* LITFLOAT_EXP LITFLOAT_TY?
  | [0-9] DECDIGIT* LITFLOAT_TY
  ;
|# 
   [(:: "\"" (:* str-char) "\"") (token-LIT_STR lexeme)]
   ["_" 'UNDERSCORE]
   ["'static" 'STATIC_LIFETIME]
   ["'self" (token-LIFETIME "'self")]
   [(:: "'" id-start (:* id-cont)) (token-LIFETIME lexeme)]
   [(eof) 'EOF]))

(define-lex-abbrevs
  ;; inadequate for unicode:
  [id-start (:or lower-alpha upper-alpha #\_)]
  [id-cont (:or id-start (:/ #\0 #\9))]
  [upper-alpha (:/ #\A #\Z)]
  [lower-alpha (:/ #\a #\z)]
  
  [one-line-comment (:: #\/ #\/ to-end-of-line)]
  [one-line-outer-doc-comment
   (:or
    (:: "///" (:* "/") non-slash-or-ws to-end-of-line)
    (:: "///" (:* "/") ws-char non-ws-char to-end-of-line))]
  [one-line-inner-doc-comment
   (:: "//!" to-end-of-line)]
  
  [block-comment
   (:: "/*" block-comment-continue block-comment-end)]
  ;; do a funny dance here to prevent "/******/"
  ;; I wonder if minus would be as fast?
  [block-outer-doc-comment
   (:: "/**" block-comment-continue 
       non-star block-comment-continue block-comment-end)]
  [block-inner-doc-comment
   (:: "/*!" block-comment-continue block-comment-end)]
  [block-comment-continue 
   (:* (:or (:~ "*") (:: (:+ "*") (:~ "*" "/"))))]
  [block-comment-end
   (:: (:+ "*") "/")]
#|
SHEBANG_LINE : {at_beginning_of_file()}? '#!' ~[\n]* '\n' -> skip ;

BINDIGIT : [0-1_] ;|#
  [dec-digit (:or #\_ (:/ #\0 #\9))]
  [hex-digit (:or dec-digit (:/ #\a #\f) (:/ #\A #\F) #\_)]
  [intlit-ty (:: (:or "u" "i") (:? (:or "8" "16" "32" "64")))]
  #|
LITFLOAT_EXP : [eE] [+-]? DECDIGIT+ ;
LITFLOAT_TY : 'f' ('32'|'64')? ;
|# 
  [escaped-char 
   (:or "n" "r" "t" #\\ #\' #\"
        (:: "x" hex-digit hex-digit)
        (:: "u" hex-digit hex-digit hex-digit hex-digit)
        (:: "U" hex-digit hex-digit hex-digit hex-digit
            hex-digit hex-digit hex-digit hex-digit))]
  [lit-char (:or (:: "'\\" escaped-char "'")
                 (:: "'" (:~) "'"))]
  [str-char (:or (:~ #\\ #\") (:: #\\ str-escape))]
  [str-escape (:or #\newline escaped-char)]
  [non-slash-or-ws (:~ #\space #\tab #\return #\newline #\/)]
  [ws-char (:or #\newline #\return #\tab #\space)]
  [non-ws-char (:~ #\newline #\return #\tab #\space)]
  [non-star (:~ #\*)]
  [to-end-of-line (:: (:* (:~ #\newline)) #\newline)]
  [rust-whitespace (:+ ws-char)]
  [comment (:or line-comment
                #;inline-comment)]
  )

