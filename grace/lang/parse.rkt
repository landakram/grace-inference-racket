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
   ; (debug "errordump")
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
    (code-sequence ((_code-sequence) (at-src (grace:code-seq $1))))

    (_code-sequence
     ((code) (list $1))
     ((code _code-sequence) (cons $1 $2)))

    (code
     ((method-declaration) $1)
     ((statement) $1)
     ((NEWLINE) (void)))

    (statement
     ((declaration NEWLINE) $1)
     ((expression NEWLINE) $1)
     ((return NEWLINE) $1)
     ((if-then NEWLINE) $1)
     ((any := expression NEWLINE) (at-src (grace:bind $1 $3))))

    (return
     ((RETURN) (at-src (grace:return "void")))
     ((RETURN expression) (at-src (grace:return $2))))

    (declaration
     ((var-declaration) $1)
     ((def-declaration) $1)
     ((type-declaration) $1)
     ((class-declaration) $1))

    (type-declaration
     ((TYPE identifier = LBRACE NEWLINE method-signatures RBRACE)
      (at-src (new grace:type:object%
                   [internal-name (grace:identifier-value (syntax->datum $2))]
                   [methods $6]))))

    (method-signatures
     ((method-name method-signature method-return-type NEWLINE)
      (list (new grace:type:method%
                 [name (string->symbol
                        (grace:identifier-value (syntax->datum $1)))]
                 [signature $2]
                 [rtype (syntax->datum $3)])))

     ((method-name method-signature method-return-type NEWLINE
                   method-signatures)
      (append (list (new grace:type:method%
                         [name (string->symbol
                                (grace:identifier-value (syntax->datum $1)))]
                         [signature $2]
                         [rtype (syntax->datum $3)])) $5)))

    (var-declaration
     ((VAR identifier type-ref)
      (at-src (grace:var-decl $2 $3 #f)))

     ((VAR identifier type-ref := expression)
      (at-src (grace:var-decl $2 $3 $5))))

    (type-ref
     ((: identifier) $2)
     (() #f))

    (def-declaration
      ((DEF identifier type-ref = expression)
       (at-src (grace:def-decl $2 $3 $5))))

    (method-declaration
     ((METHOD method-name method-return-type LBRACE method-body RBRACE NEWLINE)
      (at-src (grace:method $2 empty $5 $3)))

     ((METHOD method-name method-signature method-return-type
              LBRACE method-body RBRACE NEWLINE)
      (at-src (grace:method $2 $3 $6 $4))))

    (method-name
     ;@@@@@ TODO: allow operators @@@@@
     ((identifier) $1)

     ((identifier :=)
      (at-src
       (grace:identifier
        (format "~a:=" (grace:identifier-value (syntax->datum $1))) #f))))

    (method-signature
     ((LPAREN RPAREN) empty)
     ((LPAREN signature-list RPAREN) $2))

    (signature-list
     ((identifier) (list $1))

     ((IDENTIFIER : identifier)
      (list (at-src (grace:identifier (symbol->string $1) $3))))

     ((identifier COMMA signature-list) (append (list $1) $3))

     ((IDENTIFIER : identifier COMMA signature-list)
      (append
       (list (at-src (grace:identifier (symbol->string (quote $1)) $3))) $5)))

    (method-return-type
     ((ARROW identifier) $2)
     (() #f))

    (method-body
     ((statement method-body) (cons $1 $2))
     ((NEWLINE) (void))
     ((NEWLINE method-body) $2)
     ((statement) (list $1)))

    (expression
     ((expression + expression) (at-src (grace:expression + $1 $3)))
     ((expression - expression) (at-src (grace:expression - $1 $3)))
     ((expression * expression) (at-src (grace:expression * $1 $3)))
     ((expression / expression) (at-src (grace:expression / $1 $3)))
     ((expression % expression) (at-src (grace:expression modulo $1 $3)))
     ((expression ^ expression) (at-src (grace:expression exp $1 $3)))

     ((expression && expression) (at-src (grace:expression 'and $1 $3)))
     ((expression OR expression) (at-src (grace:expression 'or $1 $3)))

     ((expression == expression) (at-src (grace:expression equal? $1 $3)))
     ((expression < expression) (at-src (grace:expression < $1 $3)))
     ((expression > expression) (at-src (grace:expression > $1 $3)))
     ((expression <= expression) (at-src (grace:expression <= $1 $3)))
     ((expression >= expression) (at-src (grace:expression >= $1 $3)))

     ((term) $1))
    ; multi-part method names
    ;((postfixsquare) $1)

    (parenthesis-expr ((LPAREN expression RPAREN) $2))

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

    (term ((NUM) (at-src (grace:number $1)))
          ((STRING) (at-src (grace:str $1)))
          ((any) $1)
          ((- term)
           (prec UNARY)
           (at-src (grace:method-call
                    (at-src (grace:member $2 -)) empty)))
          ((+ term)
           (prec UNARY)
           (at-src (grace:method-call
                    (at-src (grace:member $2 +)) empty)))
          ; matchcase
          ; catchcase
          ((object-decl) $1))
    ; dotypeterm
    ; block
    ; array

    (identifier
     ((IDENTIFIER) (at-src (grace:identifier (symbol->string $1) #f))))

    (if-then
     ((IF LPAREN expression RPAREN THEN LBRACE if-body RBRACE)
      (at-src (grace:if-then $3 $7))))

    (class-declaration
     ((CLASS identifier LBRACE class-body RBRACE)
      (at-src (grace:class-decl $2 $4))))

    (class-body
     ((_code-sequence) $1))

    (object-decl
     ((OBJECT LBRACE object-body RBRACE) (at-src (grace:object $3)))
     ((OBJECT LBRACE RBRACE) (at-src (grace:object empty))))
    ;@@@@@ TODO: extends, etc. @@@@@

    (if-body
     ((_code-sequence) $1))

    (object-body
     ((_code-sequence) $1))

    (possibly-newline
     ((NEWLINE) (void))
     (() (void))))))


;To test lexing and parsing without typechecking, copy your program into
;open-input-string
;(define (p in) (parse (object-name in) in))
;(define a (p (open-input-string "if (true) then (var z := 4 \n)\n")))
;(display a)

;; @@@@@ DEBUGGING CODE @@@@@

;(define (p in)
;  (parse (object-name in) in))
;
;(define a (p (open-input-string "
;type Object_119 = {
;    b() -> Number
;    b:=() -> Number
;    foo(_ : Number, _ : String, _ : Boolean) -> String
;}
;
;var obj : Object_119 := object {
;    var b : Number := 2
;	method foo(x : Number, y : String, z : Boolean) -> String {
;	        var w : Boolean := z
;            print(\"World\")
;		return \"Hello\"
;	}
;}
;
;obj.foo(2, \"2\", true)
;")))
