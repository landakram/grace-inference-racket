#lang racket
(require parser-tools/yacc
         "lex.rkt"
         "helpers.rkt")
(provide simple-grace-parser)

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
(struct code-seq (c))
(struct str (s))

(define simple-grace-parser
  (parser
   (start code-sequence)
   (end EOF)
   (suppress )
   (debug "errordump")
   (error (lambda (a t v start end) 
            (raise-parse-error t v start end)))
   (src-pos)
   (tokens value-tokens op-tokens)
   (precs 
    (left - +)
    ;(left * / %)
    (left DOT)
    (nonassoc LPAREN))
   (grammar
    (code-sequence ((_code-sequence) (at-src `(code-seq ,$1))))
    (_code-sequence 
     (() empty)
     ;((statement) $1)
     ((method-declaration _code-sequence) (cons $1 $2))
     ((statement _code-sequence) (cons $1 $2))
     )
     
    (statement
     ((declaration) $1)
     ((expression) $1)                
     ((identifier := expression) (at-src `(bind ,$1 ,$3)))
  ;   ((return-stmt) $1)
     )
    (declaration 
     ((var-declaration) $1)
     ((def-declaration) $1))
    
    (var-declaration 
     ((VAR identifier) (at-src `(var-decl ,$2 #f)))
     ((VAR identifier := expression) (at-src `(var-decl ,$2 ,$4))))
    (def-declaration
     ((DEF identifier = expression) (at-src `(def-decl ,$2 ,$4))))
    (method-declaration
     ((METHOD identifier LBRACE method-body RBRACE) (at-src `(method ,$2 $4))))
    (method-body
     ((statement method-body) (cons $1 $2))
     (() empty))
    (expression
     ((identifier dotrest) (at-src `(member ,$1 ,$2)))
     ((identifier callrest) (at-src `(method-call ,$1 ,$2)))
     ((expression + expression) (at-src `(arith-exp ,+ ,$1 ,$3)))
     ((expression - expression) (at-src `(arith-exp ,- ,$1 ,$3)))
     ((term) $1)
     ; multi-part method names
     ; unparenthesized method call
     ;((postfixsquare) $1)
     ((parenthesis-expr) $1))
    (parenthesis-expr ((LPAREN expression RPAREN) $2))
    (callrest 
     ((LPAREN method-list RPAREN) $2)
     ((callrest dotrest) `(member ,$1 ,$2))
     ((LPAREN RPAREN) empty))
    (dotrest
     ((DOT identifier) $2) 
     ((DOT identifier dotrest) `(member ,$2 ,$3))
     ((DOT identifier callrest) `(method-call ,$2 ,$3))
     )
    (method-list 
     ((expression COMMA method-list) (append $1 $3))
     ((expression) (list $1)))
    (term ((NUM) (at-src `(num-exp ,$1)))
          ((STRING) (at-src `(str ,$1)))
          ((identifier) $1)
          ; matchcase
          ; catchcase
          ((object-decl) $1))
          ; dotypeterm
          ; block
          ; array
          ; prefixop
    (identifier ((IDENTIFIER) (at-src `(var-exp ,$1))))
          
    (object-decl ((OBJECT LBRACE object-body RBRACE) (at-src `(object-node ,$3)))
                 ((OBJECT LBRACE RBRACE) (at-src `(object-node empty))))
              ;extends, etc.
    (object-body 
     ((method-declaration object-body) `(cons ,$1 ,$2))
     ((statement object-body) `(cons ,$1 ,$2))
     ((method-declaration) $1)
     ((statement) $1))
    
   )
  ))

(let ((input (open-input-string "var a := \"hello\"")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))

(let ((input (open-input-string "var a := 5 + 1 - 10")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))

(let ((input (open-input-string "var x := object { var v:=1 }")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))

(let ((input (open-input-file "/Users/Mark/Desktop/minigrace/tests/t014_objectvar_test.grace")))
  (simple-grace-parser (lex-this simple-grace-lexer input)))
