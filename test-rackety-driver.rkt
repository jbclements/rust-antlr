#lang racket

(require "rust-file-list.rkt"
         "rust-lexer.rkt")

(define lexer-files (make-lexer-list "/Users/clements/tryrust"))

(map lex-file (take lexer-files 80))