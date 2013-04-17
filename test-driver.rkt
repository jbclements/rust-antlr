#lang racket

(define dev-null (open-output-file "/dev/null"
                                   #:exists 'update))

(display "abc" dev-null)
;; try parsing all files in ~/tryrust as tts

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
        #rx"src/test/run-pass/issue-2190.rs"))

(define (run-tests directory skip-list nonterm)
(for/sum ([f (in-directory directory)]
          #:when (regexp-match #px".*\\.rs$" f)
          #:unless (ormap (lambda (pat) (regexp-match pat f))
                          skip-list)
          )
  (match-define (list _1 stdin _2 stderr control)
    (process/ports 
     dev-null
     #f
     #f
     (let ([ans (~a "java org.antlr.v4.runtime.misc.TestRig Rust "
                    nonterm" -tree -encoding UTF-8 "
                    f)])
       (printf "~s\n" ans)
       ans)))
  (close-output-port stdin)
  (control 'wait)
  (define result (control 'status))
  (define errtext (first (regexp-match #px".*" stderr)))
  (close-input-port stderr)
  (when (or (not (eq? 'done-ok result))
            (not (equal? errtext #"")))
    (error 
     (format "test of file ~s failed with result ~s and stderr\n~a"
             f result errtext)))
  1))

(run-tests "/Users/clements/rust/src"
           parser-dont-try-list
           "prog")