#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

(define (make-srcloc orig-stx start-pos end-pos)
  (list (if (syntax? orig-stx) orig-stx 'no-syntax-available)
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))

;; macro to build syntax object
;; If our parser has a clause such as:
;; (foo
;;  [(FOO BAR BAZ) ...])
;; then we can get the start position of FOO with $1-start-pos,
;; it's end position with $1-end-pos, and likewise, BAR's start
;; position with $2-start-pos...
;; This very unhygenic macro keeps us from having to write that
;; long phrase over and over and over...
;; 
;; http://planet.racket-lang.org/package-source/wrturtle/pyret.plt/1/2/bsl/parse.rkt
;;
(define-syntax (b-syn stx)
  (syntax-case stx ()
    [(_ ctxt v start end)
     (with-syntax
         ([start-pos-syn (datum->syntax #'start
                                        (string->symbol
                                         (format "$~a-start-pos" 
                                                 (syntax->datum #'start))))]
          [end-pos-syn (datum->syntax #'end
                                      (string->symbol
                                       (format "$~a-end-pos" 
                                               (syntax->datum #'end))))])
       #'(datum->syntax #f
                        v
                        (list (current-source-name)
                              (position-line start-pos-syn)
                              (position-col start-pos-syn)
                              (position-offset start-pos-syn)
                              (- (position-offset end-pos-syn) (position-offset start-pos-syn)))))]))

(define current-source-name (make-parameter #f))

(define-tokens value-tokens (NUM STRING IDENTIFIER))
(define-empty-tokens op-tokens (EOF 
                                + - = := : DOT -> \;
                                LBRACE RBRACE 
                                LPAREN RPAREN 
                                LBRACKET RBRACKET
                                COMMA
                                ARROW
                                SEMICOLON
                                < >
                                NEWLINE
                                OBJECT METHOD VAR TYPE IMPORT CLASS
                                RETURN DEF INHERITS IS DIALECT
                                ))

(define-lex-abbrevs
  (CR #\015)
  (LF #\012)
  (FF #\014)
  (TAB #\011)
  (my-whitespace (:or #\space TAB FF))
  (my-newline (:or (:: CR) (:: LF) (:seq CR LF)))
  (letter (:or (:/ "a" "z") (:/ #\A #\Z) ))
  (digit (:/ #\0 #\9))
  (number (:+ digit))
  ; TODO: handle punctuation and escaped quotes
  (string (:: "\"" (:* (:or letter digit whitespace)) "\""))
  (identifier (:: letter (:* (:or letter digit #\_ #\?))))
  (keyword (:or "object" "method" "var" "type" "import" "class"
                "return" "def" "inherits" "is" "dialect")))

(define simple-grace-lexer
  (lexer-src-pos
   ((eof) (token-EOF))
   (whitespace (return-without-pos (simple-grace-lexer input-port)))
   ;(my-newline (token-NEWLINE))
   ("-" (token--))
   ("+" (token-+))
   ("=" (token-=))
   (":=" (token-:=))
   ("{" (token-LBRACE))
   ("}" (token-RBRACE))
   ("(" (token-LPAREN))
   (")" (token-RPAREN))
   ("[" (token-LBRACKET))
   ("]" (token-RBRACKET))
   ("," (token-COMMA))
   (":" (token-:))
   ("." (token-DOT))
   ("->" (token-ARROW))
   (";" (token-SEMICOLON))
   ("<" (token-<))
   (">" (token->))
   (keyword (string->symbol (string-upcase lexeme)))
   ; The call to call-with-input-string interprets the quoted string literally.
   ; Otherwise, the string is represented in racket as "\"theString\""
   (string (token-STRING (call-with-input-string lexeme read))) 
   (identifier (token-IDENTIFIER (string->symbol lexeme)))
   (number (token-NUM (string->number lexeme)))))

(struct var-decl (name value))
(struct def-decl (name value))
(struct bind (name value))
(struct num-exp (n))
(struct var-exp (i))
(struct arith-exp (op e1 e2))
(struct method-call (name args))
(struct object-node (body))
(struct method (name body))
(struct member (parent name))

(define simple-grace-parser
  (parser
   (start code-sequence)
   (end EOF)
   (error
    (lambda (tok-ok? tok-name tok-val start-pos end-pos)
      (raise-syntax-error 'grace
                          (if tok-ok?
                              (format "unexpected token: ~S" tok-name)
                              (format "invalid token: ~S" tok-name))
                          (datum->syntax false tok-val (make-srcloc false start-pos end-pos)))))
   (src-pos)
   (tokens value-tokens op-tokens)
   (precs 
    (left - +)
    ;(left * / %)
    (left DOT))
   (grammar
    (code-sequence 
     (() empty)
     ;((statement) $1)
     ((statement code-sequence) (cons $1 $2)))
     
    (statement
     ((declaration) $1)
     ((expression) $1)                
     ((identifier := expression) (b-syn #f `(bind ,$1 ,$3) 1 3))
  ;   ((return-stmt) $1)
     )
    (declaration 
     ((var-declaration) $1)
     ((def-declaration) $1)
     ((method-declaration) $1))
    
    (var-declaration 
     ((VAR IDENTIFIER) (b-syn #f `(var-decl ,$2 #f) 1 2))
     ((VAR IDENTIFIER := expression) (b-syn #f `(var-decl ,$2 ,$4) 1 4)))
    (def-declaration
     ((DEF IDENTIFIER = expression) (b-syn #f `(def-decl ,$2 ,$4) 1 4)))
    (method-declaration
     ((METHOD IDENTIFIER LBRACE method-body RBRACE) (b-syn #f `(method ,$2 $4) 1 5)))
    (method-body
     ((statement method-body) (cons $1 $2))
     (() empty))
    (expression
                ((dotcall) $1)
                ((callrest) $1)
                ; multi-part method names
                ; unparenthesized method call
                ;((postfixsquare) $1)
                ((parenthesis-expr) $1)
                ((expression + expression) (b-syn #f `(arith-exp ,+ ,$1 ,$3) 1 3))
                ((expression - expression) (b-syn #f `(arith-exp ,- ,$1 ,$3) 1 3))
                ((term) $1))
    (parenthesis-expr ((LPAREN expression RPAREN) $2))
    (callrest ((identifier LPAREN method-list RPAREN) (b-syn #f `(method-call ,$1 ,$3) 1 4)))
    (dotcall
     ((identifier DOT identifier) (b-syn #f `(member ,$1 ,$3) 1 3)) 
     ((identifier DOT dotcall) (b-syn #f `(member ,$1 ,$3) 1 3))
     ((identifier DOT callrest) (b-syn #f `(member ,$1 ,$3) 1 3)))
    (method-list 
     ((expression COMMA method-list) (b-syn #f `(cons ,$1 ,$3) 1 3))
                 ((expression) $1)
                 (() empty))
    (term ((NUM) (b-syn #f `(num-exp ,$1) 1 1))
          ((STRING) (b-syn #f $1 1 1))
          ((identifier) $1)
          ; matchcase
          ; catchcase
          ((object-decl) $1))
          ; dotypeterm
          ; block
          ; array
          ; prefixop
    (identifier ((IDENTIFIER) (b-syn #f `(var-exp ,$1) 1 1)))
          
    (object-decl ((OBJECT LBRACE object-body RBRACE) (b-syn #f `(object-node ,$3) 1 4)))
              ;extends, etc.
    (object-body 
     ((declaration object-body) $1)
     ((declaration) $1))
    )
   )
   )

(define (lex-this lexer input) (lambda () (lexer input)))

(let ((input (open-input-string "var a := \"hello\"")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))

(let ((input (open-input-string "var a := 5 + 1 - 10")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))

(let ((input (open-input-string "var x:= object { var v:=1 }")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))

(let ((input (open-input-file "/Users/Mark/Desktop/minigrace/tests/t014_objectvar_test.grace")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))
