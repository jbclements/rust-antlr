#lang racket

#|   Copyright [2013] [John Clements <clements@racket-lang.org>]

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(require "rust-lexer.rkt"
         (for-syntax "antlr-parser.rkt")
         parser-tools/lex
         parser-tools/yacc)

(provide parse-file)

(define-syntax (generate-rust-parser stx)
  (syntax-case stx ()
    [(_)
     #`(parser
        (src-pos)
        (start prog)
        (end EOF)
        (error (lambda (a name val start end)
                 (error 'antlr-parser
                        "error-vals: ~e" 
                        (list a name val start end))))
        (yacc-output "/tmp/foo.y")
        #;(debug "/tmp/grammar-debug")
        (tokens empty-toks data hack)
        (grammar
         #,@rust-rules)
        )]))

(define rust-parser
  (generate-rust-parser))

(define (parse-file f)
  (call-with-input-file f
    (lambda (port)
      (rust-parser (lambda () (rust-lexer port))))))

#;(parse-file "/tmp/f.rs")


