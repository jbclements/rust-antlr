#lang racket

(require racket/generator
         rackunit
         "rust-file-list.rkt")

(define dev-null (open-output-file "/dev/null"
                                   #:exists 'update))

;; if processing drops below this pace, indicate 
;; failure and continue with the next batch
(define MIN-LINES-PER-SECOND 50)



;; GRR: the testrig doesn't clearly signal parse failures....
;; is this line indicative of an error?
(define (is-errline? line)
  (match line
    [(regexp #px#"^/") #f]
    [(regexp #px#"^line ") #t]
    [(regexp #px#"") #f]
    [other (error "can't categorize this line: ~e" line)]))

;; return only the error lines from stderr
(define (only-errlines errtxt)
  (filter is-errline? (regexp-split #px"\n" errtxt)))

;; run the parser on all files in the given directory, skipping the 
;; ones matching the "skip-list" list of patterns, parsing using 
;; the nonterminal specified by 'nonterm', running the given number in
;; parallel, and optionally skipping all files until finding one 
;; that matches the 'start-with-pat' pattern
(define (run-tests directory skip-list nonterm num-to-run-in-parallel 
                   [start-with-pat #f])
  (define filenames (make-file-name-list directory skip-list))
  (when (empty? filenames)
    (error 'run-tests
           "empty list of files to test"))
  (define filenames2 (cond [start-with-pat
                            (dropf filenames (lambda (f)
                                               (not (regexp-match start-with-pat f))))]
                           [else filenames]))
  (when (empty? filenames2)
    (error 'run-tests
           "ran out of files while searching for pattern: ~e" start-with-pat))
  (let loop ([remaining filenames2] [failed empty])
    (cond [(empty? remaining)
           (when (not (empty? failed))
             (error 'run-tests
                    "some files timed out: ~s" failed))]
          [else
           (define-values (next-bunch new-remaining)
             (split-at remaining (min num-to-run-in-parallel (length remaining))))
           (cond [(run-bunch nonterm next-bunch)
                  (loop new-remaining failed)]
                 [else 
                  (loop new-remaining (append next-bunch failed))])])))

;; test a bunch of files at once:
(define (run-bunch nonterm bunch)
  (define total-lines (apply + (map file-lines bunch)))
  (define pre-time (current-inexact-milliseconds))
  (display (~a "Testing files containing "total-lines" lines of source code.\n"))
  (match-define (list _1 stdin _2 stderr control)
    (process/ports 
     dev-null
     #f
     #f
     (let ([ans (~a "java -Xmx2g org.antlr.v4.runtime.misc.TestRig Rust "
                    nonterm" -encoding UTF-8 "
                    (apply string-append (add-between bunch " ")))])
       (printf "~s\n" ans)
       ans)))
  (close-output-port stdin)
  (define seconds-to-wait (/ total-lines MIN-LINES-PER-SECOND))
  (define wait-thread (thread (lambda () (control 'wait))))
  (define wait-result
    (sync/timeout seconds-to-wait wait-thread))
  ;; kill the process if it times out:
  (cond 
    [(not wait-result)
     ;; On Windows, this probably won't kill the process:
     (control 'kill)
     (display
      (~a "*** TIMEOUT: processing of files aborted after waiting "(~r seconds-to-wait)" seconds (= "MIN-LINES-PER-SECOND" lines per second):\n"bunch"\n")
      (current-error-port))
     #f]
    [else
     
     (define result (control 'status))
     (define errtext (first (regexp-match #px".*" stderr)))
     (close-input-port stderr)
     (define errlines (only-errlines errtext))
     (when (or (not (eq? 'done-ok result))
               (not (empty? errlines)))
       (error 
        (format "test of files ~s failed with result ~s and stderr\n~a"
                bunch result errtext)))
     (define post-time (current-inexact-milliseconds))
     (define elapsed (/ (- post-time pre-time) 1000.0))
     (define lines-per-second (/ total-lines elapsed))
     (printf "parsing ran at ~a lines per second\n" (~r #:precision 2 lines-per-second))
     #t]))

;; this'll be slow, but probably not nearly as long as the parsing...
(define (file-lines path)
  (length (file->lines path)))


;; return true if a ends with the string sought
(define (string-ends-with a sought)
  (and (<= (string-length sought) (string-length a))
       (string=? (substring a (- (string-length a) (string-length sought)) (string-length a))
                 sought)))

(check-equal? (string-ends-with "abc" "bc") #t)
(check-equal? (string-ends-with "abc" "abc") #t)
(check-equal? (string-ends-with "abc" "zabc") #f)

(run-tests "/Users/clements/tryrust/src/"
           parser-dont-try-list
           "prog"
           8
           #;#px"zip-same-length.rs$")
;3:50:33 total 1-at-a-time
;1:27.62 8-at-a-time
;1:15.73 total 16-at-a-time
;1:13.03 total 32-at-a-time