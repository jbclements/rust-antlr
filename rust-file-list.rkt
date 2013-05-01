#lang racket

;; this file generates lists of rust files to test.
(provide make-lexer-list
         make-parser-list)

;; given the root of a rust tree, return a list of 
;; should-be-lexable files
(define (make-lexer-list directory)
  (make-file-name-list directory lexer-dont-try-list))

;; given the root of a rust tree, return a list of
;; should-be-parsable files
(define (make-parser-list directory)
  (make-file-name-list directory parser-dont-try-list))

;; construct a stream of file names for testing.
(define (make-file-name-list directory skip-list)
  (for/list ([f (in-directory directory)]
             #:when (regexp-match #px"\\.rs$" f)
             #:unless (ormap (lambda (pat) (regexp-match pat f))
                             skip-list)
             )
    (path->string f)))

;; some files actually aren't lexically legal:
(define lexer-dont-try-list
  (list #rx"compile-fail/issue-2354.rs$"
        #rx"compile-fail/issue-3820.rs$"
        #rx"compile-fail/unbalanced-doublequote.rs$"
        ;; kills the default jvm heap, sadly:
        #rx"run-pass/deep-vector2.rs$"))

(define parser-dont-try-list
  ;; just too rotted...
  (list #rx"src/libfuzzer/"
        ;; strange; empty files don't seem to work as 
        ;; I'd expect in ANTLR.
        #rx"librustc/middle/trans/block.rs"
        ;; in tests, there are going to be lots of things that don't compile...
        ;; paren in use decl:
        #rx"src/test/auxiliary/issue-2196-b.rs"
        ;; empty:
        #rx"src/test/auxiliary/issue-2196-d.rs"
        ;; doesn't compile:
        #rx"src/test/auxiliary/issue2378a.rs"
        #rx"src/test/auxiliary/issue2378b.rs"
        #rx"src/test/compile-fail/"
        ;; these have the keyword "class"... they're all xfail'ed
        #rx"src/test/run-pass/class-cast-to-trait-cross-crate.rs"
        #rx"src/test/run-pass/class-impl-parameterized-trait.rs"
        #rx"src/test/run-pass/class-implements-multiple-traits.rs"
        #rx"src/test/run-pass/class-trait-bounded-param.rs"
        ;; uses "bind"... obsolete?
        #rx"src/test/run-pass/clone-with-exterior.rs"
        ;; too big :(
        #rx"src/test/run-pass/deep-vector2.rs"
        ;; uses "loop" as an identifier
        #rx"src/test/run-pass/infinite-loops.rs"
        #rx"src/test/run-pass/int-conversion-coherence.rs"
        #rx"src/test/run-pass/issue-2101.rs"
        #rx"src/test/run-pass/issue-2190.rs"
        ;; says it isn't a test... very confusing
        #rx"src/test/run-pass/select-macro.rs"
        ;; just a sketch of what traits were going to look like.
        #rx"src/test/run-pass/traits.rs"))

