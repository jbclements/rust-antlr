#lang racket

(define dev-null (open-output-file "/dev/null"
                                   #:exists 'update))

(display "abc" dev-null)
;; try parsing all files in ~/tryrust as tts

(time
 (for/sum ([f (in-directory "/Users/clements/tryrust")]
           #:when (regexp-match #px".*\\.rs$" f)
       ;;[i (in-naturals)]
       )
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