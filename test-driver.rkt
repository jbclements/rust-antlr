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
        #rx"librustc/middle/trans/block.rs"))

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

(run-tests "/Users/clements/tryrust/src/librustc/middle"
           parser-dont-try-list
           "prog")