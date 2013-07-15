#lang racket

(require parser-tools/yacc
         syntax/readerr
         "lex.rkt"
         "helpers.rkt"
         "astR.rkt")

(provide parse)

;(define (empty-line)
;  (datum->syntax #f (list)))

;(struct grace:newline ())

;(define newline
;  (at-src (grace:newline)))

(define (parse src-name in)
  (parameterize ([current-source src-name])
    (simple-grace-parser (lambda () (simple-grace-lexer in)))))

(define simple-grace-parser
  (parser
   (start code-sequence)

   (end EOF)

   (error (lambda (a t v start end)
            (raise-parse-error t v start end)))

   (src-pos)

   (tokens value-tokens op-tokens)

   (precs
    (left && OR)
    (left < <= > >= ==)
    (left * / %)
    (right !)
    (right ^)
    (left DOT)
    (nonassoc UNARY)
    (nonassoc LPAREN RPAREN)
    (left METHODCALL))

   (grammar
    (code-sequence ((_code-sequence) (at-src (grace:code-seq $1))))

    (_code-sequence
     ((code) (list $1))
     ((code _code-sequence) (cons $1 $2)))

    (code
     ((method-declaration) $1)
     ((statement) $1)
     ((NEWLINE) (at-src (grace:newline))))

    (statement
     ((declaration NEWLINE) $1)
     ((expression NEWLINE) $1)
     ((return NEWLINE) $1)
     ((if-then-else NEWLINE) $1)
     ((any := expression NEWLINE) (at-src (grace:bind $1 $3))))
     ;((NEWLINE) (at-src (grace:newline))))

    (return
     ;; TODO: Change to keyword.
     ((RETURN) (at-src (grace:return "Done")))
     ((RETURN expression) (at-src (grace:return $2))))

    (declaration
     ((type-definition) $1)
     ((var-declaration) $1)
     ((def-declaration) $1)
     ((class-declaration) $1))

    ;; Type definitions
    (type-definition
     ((TYPE identifier = LBRACE typedef-body RBRACE)
      (at-src (grace:type-def $2 $5))))

    ;; Body is made up of method definitions or newlines.
    (typedef-body
     ((method-definition typedef-body) (cons $1 $2))
     ((NEWLINE) (at-src (grace:newline)))
     ((NEWLINE typedef-body) $2)
     ((method-definition) (list $1)))

    (var-declaration
     ((VAR identifier type-ref)
      (at-src (grace:var-decl $2 $3 #f)))
     ((VAR identifier type-ref := expression)
      (at-src (grace:var-decl $2 $3 $5))))

    (def-declaration
      ((DEF identifier type-ref = expression)
       (at-src (grace:def-decl $2 $3 $5))))

    (type-ref
     ((: identifier) $2)
     (() #f))

    ;; A method definition just gives the method's signature and rtype inside a typedef
    (method-definition
     ((method-name method-signature method-return-type NEWLINE)
      (grace:method-def $1 $2 $3)))

    ;; A method declaration is the method's implementation
    (method-declaration
     ((METHOD method-name method-return-type LBRACE method-body RBRACE NEWLINE)
      (at-src (grace:method $2 empty $5 $3)))
     ((METHOD method-name method-signature method-return-type LBRACE method-body RBRACE NEWLINE)
      (at-src (grace:method $2 $3 $6 $4))))

    (method-name
     ((identifier) $1)
     ((identifier :=)
      (at-src
       (grace:identifier
        (format "~a:=" (grace:identifier-value (syntax->datum $1))) #f))))

    ;; TODO: Extend method-signatures to take multipart methods.
    (method-signature
     ((LPAREN RPAREN) empty)
     ((LPAREN signature-list RPAREN) $2))

    (signature-list
     ((identifier)
      (list $1))
     ((IDENTIFIER : identifier)
      (list (at-src (grace:identifier (symbol->string $1) $3))))
     ((identifier COMMA signature-list)
      (append (list $1) $3))
     ((IDENTIFIER : identifier COMMA signature-list)
      (append (list (at-src (grace:identifier (symbol->string (quote $1)) $3))) $5)))

    (method-return-type
     ((ARROW identifier) $2)
     (() #f))

    (method-body
     ((statement method-body) (cons $1 $2))
     ((NEWLINE) (at-src (grace:newline)))
     ((NEWLINE method-body) $2)
     ((statement) (list $1)))

    (expression
     ((expression + expression) (at-src (grace:expression 'plus $1 $3)))
     ((expression - expression) (at-src (grace:expression 'minus $1 $3)))
     ((expression * expression) (at-src (grace:expression 'mult $1 $3)))
     ((expression / expression) (at-src (grace:expression 'div $1 $3)))
     ((expression % expression) (at-src (grace:expression 'modulo $1 $3)))
     ((expression ^ expression) (at-src (grace:expression 'exp $1 $3)))

     ((expression && expression) (at-src (grace:expression 'and $1 $3)))
     ((expression OR expression) (at-src (grace:expression 'or $1 $3)))

     ((expression == expression) (at-src (grace:expression 'equal? $1 $3)))
     ((expression < expression) (at-src (grace:expression 'less-than $1 $3)))
     ((expression > expression) (at-src (grace:expression 'greater-than $1 $3)))
     ((expression <= expression) (at-src (grace:expression 'less-than-equal $1 $3)))
     ((expression >= expression) (at-src (grace:expression 'greather-than-equal $1 $3)))

     ((expression ++ expression) (at-src (grace:expression 'concat $1 $3)))
     ((term) $1))

    (parenthesis-expr
     ((LPAREN expression RPAREN) $2))

    (id-or-member
     ((identifier) $1)
     ((parenthesis-expr) $1)
     ((id-or-member DOT identifier)
      (prec METHODCALL) (at-src (grace:member $1 $3))))

    (call
     ((id-or-member callrest) (at-src (grace:method-call $1 $2)))
     ((call callrest) (prec METHODCALL) (at-src (grace:method-call $1 $2))))

    (any-dot
     ((any DOT id-or-member) (at-src (grace:member $1 $3)))
     ((id-or-member) (prec UNARY) $1)
     ((call) (prec UNARY) $1))

    (any-call
     ((any-dot callrest) (at-src (grace:method-call $1 $2)))
     ((any-call callrest) (at-src (grace:method-call $1 $2))))

    (any
     ((any-call) $1)
     ((any-dot) $1))

    (callrest
     ((LPAREN method-list RPAREN) $2)
     ((LPAREN RPAREN) empty))

    (method-list
     ((expression COMMA method-list) (append (list $1) $3))
     ((expression) (list $1)))

    (term
     ((NUM) (at-src (grace:number $1)))
     ((STRING) (at-src (grace:str $1)))
     ((any) $1)
     ((- term)
      (prec UNARY)
      (at-src (grace:method-call (at-src (grace:member $2 -)) empty)))
     ((+ term)
      (prec UNARY)
      (at-src (grace:method-call (at-src (grace:member $2 +)) empty)))
     ((object-decl) $1))

    (identifier
     ((IDENTIFIER) (at-src (grace:identifier (symbol->string $1) #f))))

    ; TODO: elseif. Recursive else-list?
    (if-then-else
     ((IF LPAREN expression RPAREN THEN LBRACE if-body RBRACE ELSE LBRACE if-body RBRACE)
      (at-src (grace:if-then-else $3 $7 $11)))
     ((IF LPAREN expression RPAREN THEN LBRACE if-body RBRACE)
      (at-src (grace:if-then-else $3 $7 empty))))

    ; TODO: Should take multipart constructor (see: method-declaration)
    (class-declaration
     ((CLASS identifier DOT identifier method-signature LBRACE class-body RBRACE)
      (at-src (grace:class-decl $2 $4 $5 $7))))

    (class-body
     ((_code-sequence) $1))

    (object-decl
     ((OBJECT LBRACE object-body RBRACE)
      (at-src (grace:object $3)))
     ((OBJECT LBRACE RBRACE)
      (at-src (grace:object empty))))

    (if-body
     ((_code-sequence) $1))

    (object-body
     ((_code-sequence) $1))

    (possibly-newline
     ;; TODO: possibly reinsert below line, was giving strange error.
     ;(() (at-src (grace:newline)))
     ((NEWLINE) (at-src (grace:newline)))))))
     ;((NEWLINE) (at-src (grace:newline)))
     ;(() (at-src (grace:newline)))))))
     ;(() (void))))))
