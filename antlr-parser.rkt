#lang racket

(require rackunit
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         syntax/readerr
         racket/generator)

(provide rust-rules)

(define lines 
  (file->lines "/Users/clements/rust-antlr/Rust-massaged.g4")
  #;(list "a" "b" "/// <foo>" ))

(define ((parse-line tags) line)
  (match line
    [(regexp #px"^///\\s*<\\s*(\\w+)\\s*>\\s*$"
             (list _ tag))
     (cond [(member (string->symbol tag) tags)
            (list 'start (string->symbol tag))]
           [else '(ignored-tag)])]
    [(regexp #px"^///\\s*<\\s*/(\\w+)\\s*>\\s*$"
             (list _ tag))
     (cond [(member (string->symbol tag) tags)
            (list 'end (string->symbol tag))]
           [else '(ignored-tag)])]
    [other (list 'line line)]))

(define line-stream
  (sequence->generator (map (parse-line '(grammar)) lines)))

(define (line-parsing-loop stream in-tag)
  (define first-line (stream))
  (match first-line
    [(? void? v) 
     (cond [in-tag
            (error 'line-parsing-loop
                   "expected closing tag for ~e, found EOF"
                   in-tag)]
           [else empty])]
    [(list 'ignored-tag)
     (line-parsing-loop stream in-tag)]
    [(list 'line line) 
     (cons line
           (line-parsing-loop stream in-tag))]
    [(list 'start new-tag)
     (cons (cons new-tag
                 (line-parsing-loop stream new-tag))
           (line-parsing-loop stream in-tag))]
    [(list 'end ending-tag)
     (cond [(eq? ending-tag in-tag)
            empty]
           [else
            (error 'line-parsing-loop
                   "expected ending tag ~a, found ~a"
                   in-tag ending-tag)])]
    [else (error 'line-parsing-loop "unexpected line: ~e" 
                 first-line)]))

(define the-grammar-text
  (apply
   string-append
   (add-between
    (match 
        (filter (lambda (x)
                  (and (list? x) (equal? (first x) 'grammar)))
                (line-parsing-loop line-stream #f))
      [(list g) (rest g)]
      [other (error 'foo "expected just one grammar")])
    "\n")))

;; okay, now we've got the grammar part...


(define-tokens data (TERM NONTERM))
(define-empty-tokens delim 
  (COLON OR SEMI LPAREN RPAREN HUH STAR PLUS EOF))

(define antlr-lexer
  (lexer-src-pos
   
   ;; Skip comments, without accumulating extra position information
   [(:or antlr-whitespace comment) 
    (return-without-pos (antlr-lexer input-port))]
   [upper-ident (token-TERM lexeme)]
   [lower-ident (token-NONTERM lexeme)]
   [#\| 'OR]
   [#\; 'SEMI]
   [#\( 'LPAREN]
   [#\) 'RPAREN]
   [#\? 'HUH]
   [#\* 'STAR]
   [#\: 'COLON]
   [#\+ 'PLUS]
   [(eof) 'EOF]))




(define-lex-abbrevs
  [lower-ident (:: lower-alpha (:* (:or alpha #\_ digit)))]
  [upper-ident (:+ (:or (:/ #\A #\Z) #\_ digit))]
  [lower-alpha (:/ #\a #\z)]
  [alpha (:or lower-alpha (:/ #\A #\Z))]
  [digit (:/ #\0 #\9)]
  [antlr-whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [comment (:or line-comment inline-comment)]
  [line-comment (:: #\/ #\/ (:* (:~ #\newline)) #\newline)]
  [inline-comment (:: #\/ #\* 
                      (:* (:or (:~ #\*)
                               (:: #\* (:~ #\/ #\*))))
                      (:+ #\*) #\/)]
  )

(define antlr-parser
  (parser
   (src-pos)
   
   (start s)
   (end EOF)
   (error (lambda (a name val start end)
            (error 'antlr-parser
                   "error-vals: ~e" 
                   (list a name val start end))))
   #;(debug "/tmp/grammar-debug")
   (tokens data delim)
   
   
   (grammar
    (s [(rule s) (cons $1 $2)]
       [() empty])
    (rule [(NONTERM COLON pats SEMI)
           (let ()
             (define name $1)
             (fence-out-name name)
             (list
              name
              ((rewrite-pat-to-or-of-seqs $1)
               (cons 'or $3))))])
    (pats [(pat-seq OR pats) (cons (cons 'seq $1) $3)]
          [(pat-seq) (list (cons 'seq $1))])
    (pat-seq [(one-pat pat-seq) (cons $1 $2)]
             [() empty])
    (one-pat [(term-or-nonterm) $1]
             [(LPAREN pats RPAREN) (cons 'or $2)]
             [(one-pat STAR) (list 'star $1)]
             [(one-pat HUH) (list 'huh $1)]
             [(one-pat PLUS) (list 'plus $1)])
    (term-or-nonterm [(TERM) $1]
                     [(NONTERM) $1]))))
  
;; this would be perfect for monadic style... but 
;; I'll just use mutation instead.
(define aux-defn-bucket (make-hash))
(define (get-aux-defn-name base)
  (define new-idx
    (let loop ([idx 1])
      (define candidate (~a base "_" idx))
      (cond [(hash-has-key? aux-defn-bucket candidate)
             (loop (add1 idx))]
            [else idx])))
  (~a base "_" new-idx))
(define (fence-out-name name)
  (cond [(hash-has-key? aux-defn-bucket name)
         (error 'fence-out-name
                "too late! name ~e already appears in the table"
                name)]
        [(hash-set! aux-defn-bucket name #f)]))
(define (reset-aux-defns!)
  (set! aux-defn-bucket (make-hash)))

;; NOTE! I'VE ONLY IMPLEMENTED THE ONES I NEEDED, HERE...

(define ((rewrite-pat-to-nonterm name) orig-pat)
  (define pat (maybe-simplify-pat orig-pat))
  (match pat
    [(? string? s) s]
    ;; special-case for 1-ary case makes things 
    ;; *much* more readabale...
    [(list 'star (? string? s))
     (define new-name (get-aux-defn-name (~a name "_star")))
     (hash-set! aux-defn-bucket
                new-name
                `(() 
                  (,s ,new-name)))
     new-name]
    [(list 'star pat)
     (define lifted-rhs-seq ((rewrite-pat-to-seq name) pat))
     (define new-name (get-aux-defn-name (~a name "_star")))
     (hash-set! aux-defn-bucket
                new-name
                `(() 
                  (,@lifted-rhs-seq ,new-name)))
     new-name]
    ;; special-case to improve readability
    [(list 'plus (? string? s))
     (define new-name (get-aux-defn-name (~a name "_plus")))
     (hash-set! aux-defn-bucket
                new-name
                `((,s)
                  (,s ,new-name)))
     new-name]
    [(list 'huh (? string? s))
     (define new-name (get-aux-defn-name (~a name "_star")))
     (hash-set! aux-defn-bucket
                new-name
                `(() 
                  (,s)))
     new-name]
    [(list 'huh pat)
     (define lifted-rhs-seq ((rewrite-pat-to-seq name) pat))
     (define new-name (get-aux-defn-name (~a name "_huh")))
     (hash-set! aux-defn-bucket
                new-name
                `(()
                  ,lifted-rhs-seq))
     new-name]
    [(cons 'seq pats)
     (define new-name (get-aux-defn-name name))
     (define rewritten ((rewrite-pat-to-or-of-seqs name)
                        (cons 'seq pats)))
     (hash-set! aux-defn-bucket new-name rewritten)
     new-name]
    [(cons 'or pats)
     (define new-name (get-aux-defn-name name))
     (define rewritten ((rewrite-pat-to-or-of-seqs name)
                        (cons 'or pats)))
     (hash-set! aux-defn-bucket new-name rewritten)
     new-name]
    [other (error 'abc "unimplemented: ~e" other)]))

(define ((rewrite-pat-to-or-of-seqs name) pat)
  (match pat
    [(cons 'or pats)
     (map (rewrite-pat-to-seq name) pats)]
    [(cons 'seq pats)
     (list ((rewrite-pat-to-seq name) pat))]))

(define ((rewrite-pat-to-seq name) pat)
  (match pat
    [(cons 'seq pats)
     (map (rewrite-pat-to-nonterm name) pats)]
    [(? string? s)
     (list s)]))

;; get rid of superfluous ors and seqs
(define (maybe-simplify-pat pat)
  (match pat
    [(? string? s) s]
    [(list 'or pat) (maybe-simplify-pat pat)]
    [(list 'seq pat) (maybe-simplify-pat pat)]
    [(cons kind pats) (cons kind (map maybe-simplify-pat pats))]))

(check-equal? ((rewrite-pat-to-nonterm "bogo") "attrs_and_vis")
              "attrs_and_vis")
(check-equal? ((rewrite-pat-to-nonterm "bogo") '(star "frongy"))
              "bogo_star_1")
(check-equal? (hash-ref aux-defn-bucket "bogo_star_1")
              '(() ("frongy" "bogo_star_1")))

(reset-aux-defns!)


(define (post-process-rules grammar)
  (for/list ([rule grammar])
    (match-define (list name rhses) rule)
    (cons (string->symbol name)
          (map post-process rhses))))

(define (post-process los)
  (list (map string->symbol los) #f))


(define (rs ip)
  (port-count-lines! ip)
  (antlr-parser (lambda () (antlr-lexer ip))))

(define grammar-port (open-input-string the-grammar-text))

(define rust-rules
  (post-process-rules
   (append
    (rs grammar-port)
    (filter (lambda (rule) (not (false? (second rule))))
            (hash-map aux-defn-bucket list)))))

