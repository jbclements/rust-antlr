#lang racket

(require racket/generator)

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

;; construct a stream of file names for testing.
(define (make-file-name-stream directory skip-list)
  (generator ()
   (for ([f (in-directory directory)]
             #:when (regexp-match #px".*\\.rs$" f)
             #:unless (ormap (lambda (pat) (regexp-match pat f))
                             skip-list)
             )
     (yield (path->string f)))))

(define a (make-file-name-stream "/Users/clements/rust/src"
           parser-dont-try-list))


(define (generator-take generator n)
  (cond [(= n 0) empty]
        [else
         (define next (generator))
         (cond [(void? next) empty]
               [(cons next (generator-take generator (sub1 n)))])]))

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

(define (run-tests directory skip-list nonterm num-to-run-in-parallel)
  (define filename-generator (make-file-name-stream directory skip-list))
  (let loop ()
    (define next-bunch (generator-take filename-generator num-to-run-in-parallel))
    (cond [(empty? next-bunch) #f]
          [else
           (match-define (list _1 stdin _2 stderr control)
             (process/ports 
              dev-null
              #f
              #f
              (let ([ans (~a "java -Xmx2g org.antlr.v4.runtime.misc.TestRig Rust "
                             nonterm" -encoding UTF-8 "
                             (apply string-append (add-between next-bunch " ")))])
                (printf "~s\n" ans)
                ans)))
           (close-output-port stdin)
           (control 'wait)
           (define result (control 'status))
           (define errtext (first (regexp-match #px".*" stderr)))
           (close-input-port stderr)
           (define errlines (only-errlines errtext))
           (when (or (not (eq? 'done-ok result))
                     (not (empty? errlines)))
             (error 
              (format "test of files ~s failed with result ~s and stderr\n~a"
                      next-bunch result errtext)))
           (loop)])))

(run-tests "/Users/clements/rust/src"
           parser-dont-try-list
           "prog"
           8)
;3:50:33 total 1-at-a-time
;1:27.62 8-at-a-time
;1:15.73 total 16-at-a-time
;1:13.03 total 32-at-a-time