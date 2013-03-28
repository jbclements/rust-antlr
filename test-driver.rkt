#lang racket

(define dev-null (open-output-file "/dev/null"
                                   #:exists 'update))

(display "abc" dev-null)
;; try parsing all files in ~/tryrust as tts

;; some files actually aren't lexically legal:
(define dont-try-list
  (list #rx"compile-fail/issue-2354.rs$"
        #rx"compile-fail/issue-3820.rs$"
        #rx"compile-fail/unbalanced-doublequote.rs$"
        ;; kills the default jvm heap, sadly:
        #rx"run-pass/deep-vector2.rs$"))

(time
 (for/sum ([f (in-directory "/Users/clements/tryrust")]
           #:when (regexp-match #px".*\\.rs$" f)
           #:unless (ormap (lambda (pat) (regexp-match pat f)) dont-try-list))
   (match-define (list _1 stdin _2 stderr control)
       (process/ports 
        dev-null
        #f
        #f
        (let ([ans (~a "java org.antlr.v4.runtime.misc.TestRig Rust tts -tree -encoding UTF-8 "
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