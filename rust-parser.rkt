#lang racket

(require "rust-lexer.rkt"
         (for-syntax "antlr-parser.rkt")
         parser-tools/lex
         parser-tools/yacc)

#;(require rackunit
         (prefix-in : parser-tools/lex-sre)
         
         syntax/readerr)

(define-syntax (generate-rust-parser stx)
  (syntax-case stx ()
    [(_)
     #`(parser
        (src-pos)
        (start tts)
        (end EOF)
        (error (lambda (a name val start end)
                 (error 'antlr-parser
                        "error-vals: ~e" 
                        (list a name val start end))))
        #;(debug "/tmp/grammar-debug")
        (tokens empty-toks data hack)
        (grammar
         ;; urg, I didn't expect that to work...
         (tts ((tts*_1) #f))
         #,@(datum->syntax
             #'stx
             (magically-create-rules))))]))

(generate-rust-parser)