#lang racket

(require "rust-file-list.rkt"
         "rust-lexer.rkt"
         "rust-parser.rkt")

#;(define lexer-files (make-lexer-list "/Users/clements/tryrust"))

#;(for ([f lexer-files]
      [i (in-naturals)])
  (when (= (modulo i 100) 0)
    (printf "processed ~a files\n" i))
  (lex-file f))


(define parser-files (make-parser-list "/Users/clements/tryrust"))

(for ([f parser-files]
      [i 8 #;(in-naturals)])
  (when (= (modulo i 100) 0)
    (printf "processed ~a files\n" i))
  (parse-file f))