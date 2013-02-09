#lang racket
(require parser-tools/yacc
         syntax/readerr
         "lex.rkt"
         "helpers.rkt"
         "ast.rkt")
(provide parse)

(define (parse src-name in)
  (parameterize ((current-source src-name))
    (simple-grace-parser (lambda () (simple-grace-lexer in)))))
 
(define simple-grace-parser
  (parser
   (start code-sequence)
   (end EOF)
   (suppress )
   ;(debug "errordump")
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
    (code-sequence ((_code-sequence) (at-src `(grace:code-seq ,$1))))
    (_code-sequence 
     (() `empty)
     ;((statement) $1)
     ((method-declaration _code-sequence) `(cons ,$1 ,$2))
     ((statement _code-sequence) `(cons ,$1 ,$2))
     )
     
    (statement
     ((declaration) $1)
     ((expression) $1)                
     ((identifier := expression) (at-src `(grace:bind ,$1 ,$3))))
  ;   ((return-stmt) $1)
     
    (declaration 
     ((var-declaration) $1)
     ((def-declaration) $1))
    
    (var-declaration 
     ((VAR identifier type-ref) (at-src `(grace:var-decl ,$2 ,$3 #f)))
     ((VAR identifier type-ref := expression) (at-src `(grace:var-decl ,$2 ,$3 ,$5))))
    (type-ref
     ((: identifier) $2)
     (() #f))
    (def-declaration
     ((DEF identifier type-ref = expression) (at-src `(grace:def-decl ,$2 ,$3 ,$5))))
    (method-declaration
     ((METHOD identifier LBRACE method-body RBRACE) (at-src `(grace:method ,$2 ,$4))))
    (method-body
     ((statement method-body) `(cons ,$1 ,$2))
     (() `empty))
    (expression
     ((identifier dotrest) (at-src `(grace:member ,$1 ,$2)))
     ((identifier callrest) (at-src `(grace:method-call $1 $2)))
     ((expression + expression) (at-src `(grace:arith-exp + ,$1 ,$3)))
     ((expression - expression) (at-src `(grace:arith-exp - ,$1 ,$3)))
     ((term) $1)
     ; multi-part method names
     ; unparenthesized method call
     ;((postfixsquare) $1)
     ((parenthesis-expr) $1))
    (parenthesis-expr ((LPAREN expression RPAREN) $2))
    (callrest 
     ((LPAREN method-list RPAREN) $2)
     ((callrest dotrest) (at-src `(grace:member ,$1 ,$2)))
     ((LPAREN RPAREN) `empty))
    (dotrest
     ((DOT identifier) $2) 
     ((DOT identifier dotrest) (at-src `(grace:member ,$2 ,$3)))
     ((DOT identifier callrest) (at-src `(grace:method-call ,$2 ,$3)))
     )
    (method-list 
     ((expression COMMA method-list) `(append ,$1 ,$3))
     ((expression) `(list ,$1)))
    (term ((NUM) (at-src `(grace:num-exp ,$1)))
          ((STRING) (at-src `(grace:str ,$1)))
          ((identifier) $1)
          ; matchcase
          ; catchcase
          ((object-decl) $1))
          ; dotypeterm
          ; block
          ; array
          ; prefixop
    (identifier ((IDENTIFIER) (at-src `(grace:identifier (symbol->string (quote ,$1)) #f))))
          
    (object-decl ((OBJECT LBRACE object-body RBRACE) (at-src `(grace:object ,$3)))
                 ((OBJECT LBRACE RBRACE) (at-src `(grace:object empty))))
              ;extends, etc.
    (object-body 
     ((method-declaration object-body) `(cons ,$1 ,$2))
     ((statement object-body) `(cons ,$1 ,$2))
     ((method-declaration) $1)
     ((statement) $1))
   )))
