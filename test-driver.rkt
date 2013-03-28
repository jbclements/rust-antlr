#lang racket

(define dev-null (open-output-file "/dev/null"
                                   #:exists 'update))

(display "abc" dev-null)
;; try parsing all files in ~/tryrust as tts

(for/sum ([file (in-directory "/Users/clements/tryrust")]
          #:when (regexp-match #px".*\\.rs" file)
          [i (in-naturals)])
  (when (= 0 (modulo i 10))
    (printf "i: ~s\n" i))
  (match-define (list _1 stdin _2 stderr control)
    (process/ports 
     dev-null
     #f
     #f
     (~a "java org.antlr.v4.runtime.misc.TestRig Rust tts -tree "
         file)))
  (close-output-port stdin)
  (control 'wait)
  (define result (control 'status))
  (define errtext (first (regexp-match #px".*" stderr)))
  (close-input-port stderr)
  (when (or (not (eq? 'done-ok result))
            (not (equal? errtext #"")))
    (error 
     (format "test of file ~s failed with result ~s and stderr\n~a"
             file result errtext)))
  1)