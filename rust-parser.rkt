#lang racket

(require "rust-lexer.rkt"
         (for-syntax "antlr-parser.rkt")
         parser-tools/lex
         parser-tools/yacc)

(provide parse-file)

(define-syntax (generate-rust-parser stx)
  (syntax-case stx ()
    [(_)
     #`(parser
        (src-pos)
        (start prog)
        (end EOF)
        (error (lambda (a name val start end)
                 (error 'antlr-parser
                        "error-vals: ~e" 
                        (list a name val start end))))
        (debug "/tmp/grammar-debug")
        (tokens empty-toks data hack)
        (grammar
         #,@rust-rules)
        )]))

(define rust-parser
  (generate-rust-parser))

(define (parse-file f)
  (call-with-input-file f
    (lambda (port)
      (rust-parser (lambda () (rust-lexer port))))))

(parse-file "/tmp/f.rs")


