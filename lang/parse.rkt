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
   ;(suppress )
   (debug "errordump")
   (error (lambda (a t v start end) 
            (raise-parse-error t v start end)))
   (src-pos)
   (tokens value-tokens op-tokens)
   (precs 
    (left && OR)
    (left < <= > >= ==)
    (left - +)
    (left * / %)
    (right !)
    (right ^)
    (left DOT)
    (nonassoc UNARY)
    (nonassoc LPAREN RPAREN)
    (left METHODCALL))
   (grammar
    (code-sequence ((_code-sequence) (at-src `(grace:code-seq ,$1))))
    (_code-sequence 
     ((code) $1)
     ((code _code-sequence) `(cons ,$1 ,$2)))
    (code
     ((method-declaration) $1)
     ((statement) $1)
     ((NEWLINE) `empty))
    (statement
     ((declaration NEWLINE) $1)
     ((expression NEWLINE) $1)                
     ((identifier := expression NEWLINE) (at-src `(grace:bind ,$1 ,$3))))
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
     ((METHOD identifier method-return-type LBRACE method-body RBRACE NEWLINE) (at-src `(grace:method ,$2 empty ,$5 ,$3)))
     ((METHOD identifier method-signature method-return-type LBRACE method-body RBRACE NEWLINE) (at-src `(grace:method ,$2 ,$3 ,$6 ,$4)))
     )
    (method-signature
     ((LPAREN RPAREN) `empty) 
     ((LPAREN signature-list RPAREN) $2))
    (signature-list
     ((identifier) `(list ,$1))
     ((IDENTIFIER : identifier) `(list (at-src `(grace:identifier (symbol->string (quote ,$1)) ,$3))))
     ((identifier COMMA signature-list) `(append (list,$1) ,$3))
     ((IDENTIFIER : identifier COMMA signature-list) `(append (list ,(at-src `(grace:identifier (symbol->string (quote ,$1)) ,$3))) ,$5)))
    (method-return-type
     ((ARROW identifier) $2)
     (() `#f))
    (method-body
     ((statement method-body) `(cons ,$1 ,$2))
     (() `empty))
    (expression
     ((any-members-or-calls) $1)
    ; ((identifier dotrest) (at-src `(grace:member ,$1 ,$2)))
 ;    ((identifier callrest dotrest) (at-src `(grace:member ,(at-src `(grace:method-call ,$1 ,$2)) ,$3)))
 ;    ((identifier dotrest) (at-src `(grace:member ,$1 ,$2)))
 ;    ((identifier dotrest callrest) (at-src `(grace:method-call ,(at-src `(grace:member ,$1 ,$2)) ,$3)))
     ((expression + expression) (at-src `(grace:expression + ,$1 ,$3)))
     ((expression - expression) (at-src `(grace:expression - ,$1 ,$3)))
     ((expression * expression) (at-src `(grace:expression * ,$1 ,$3)))
     ((expression / expression) (at-src `(grace:expression / ,$1 ,$3)))
     ((expression % expression) (at-src `(grace:expression modulo ,$1 ,$3)))
     ((expression ^ expression) (at-src `(grace:expression expr ,$1 ,$3)))
     
     ((expression && expression) (at-src `(grace:expression 'and ,$1 ,$3)))
     ((expression OR expression) (at-src `(grace:expression 'or ,$1 ,$3)))
     
     ((expression == expression) (at-src `(grace:expression equals? ,$1 ,$3)))
     ((expression < expression) (at-src `(grace:expression < ,$1 ,$3)))
     ((expression > expression) (at-src `(grace:expression > ,$1 ,$3)))
     ((expression <= expression) (at-src `(grace:expression <= ,$1 ,$3)))
     ((expression >= expression) (at-src `(grace:expression >= ,$1 ,$3)))
     

          
     ((term) $1)
     ; multi-part method names
     ;((postfixsquare) $1)
     ((parenthesis-expr) $1))
    (parenthesis-expr ((LPAREN expression RPAREN) $2))
   ; (id-callrest
   ;  ((identifier callrest) (at-src `(grace:method-call ,$1 ,$2)))
   ;  ((id-callrest callrest) (at-src `(grace:method-call ,$1 ,$2))))
    
    ;; Handle arbitrarily many member accesses
    ;; i.e. a.b.c.d.e...z
    (member
     ((identifier DOT identifier) (at-src `(grace:member ,$1 ,$3)))
     ((parenthesis-expr DOT identifier) (at-src `(grace:member ,$1 ,$3)))
     ((member DOT identifier) (at-src `(grace:member ,$1 ,$3))))
    ;; Handle arbitrarily many method calls
    ;; i.e. a()()()()()...()
    (call
     ((identifier callrest) (at-src `(grace:method-call ,$1 ,$2)))
     ((parenthesis-expr callrest) (at-src `(grace:method-call ,$1 ,$2)))
     ((call callrest) (at-src `(grace:method-call ,$1 ,$2))))
    ;; Handle arbitrarily many member method calls
    ;; i.e. a.b.c.d.e...z()()()()()()...()
    (member-call 
     ((member callrest) (at-src `(grace:method-call ,$1 ,$2)))
     ((member-call callrest) (at-src `(grace:method-call ,$1 ,$2))))
    ;; Handle calls or member access that begin with a call
    ;; i.e. a().b...
    (call-member-or-call
     ((call DOT identifier) (at-src `(grace:member ,$1 ,$3)))
     ((call DOT identifier callrest) (at-src `(grace:method-call ,(at-src `(grace:member ,$1 ,$3)) ,$4)))
     ((call-member-or-call DOT identifier) (at-src `(grace:member ,$1 ,$3)))
     ((call-member-or-call DOT identifier callrest) (at-src `(grace:method-call ,$1 ,(at-src `(grace:member ,$1 ,$3))))))
    ;; Handle arbitrarily many calls that begin with 
    ;; some number of member accesses and then a call.
    (member-call-member-call
     ((member-call DOT identifier callrest) (at-src `(grace:method-call ,$1 ,(at-src `(grace:member ,$1 ,$3)))))
     ((member-call-member-call callrest) (at-src `(grace:method-call ,$1 ,$2))))                    
    ;; Handle arbitrarily many calls or members 
    ;; that begin with some number of member accesses 
    ;; and then a call.
    (member-call-member-or-call
     ((member-call DOT identifier) (at-src `(grace:member ,$1 ,$3)))
     ((member-call-member-call) $1)
     ((member-call-member-or-call DOT identifier) (at-src `(grace:member ,$1 ,$3)))
     ((member-call-member-or-call DOT identifier callrest) (at-src `(grace:method-call ,$1 ,(at-src `(grace:member ,$1 ,$3))))))
    (any-members-or-calls
     ((member-call-member-or-call) $1)
     ((call-member-or-call) $1)
     ((member-call) $1)
     ((member) $1)
     ((call) $1))
    (callrest 
     ((LPAREN method-list RPAREN) $2)
     ((LPAREN RPAREN) `empty))
    
    (method-list 
     ((expression COMMA method-list) `(append ,$1 ,$3))
     ((expression) `(list ,$1)))
    
    (term ((NUM) (at-src `(grace:number ,$1)))
          ((STRING) (at-src `(grace:str ,$1)))
          ((identifier) $1)
          ((- term) 
           (prec UNARY) 
           (at-src `(grace:method-call 
                     ,(at-src `(grace:member ,$2 -)) empty)))
          ; matchcase
          ; catchcase
          ((object-decl) $1))
          ; dotypeterm
          ; block
          ; array
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
