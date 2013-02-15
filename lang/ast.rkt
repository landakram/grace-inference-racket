#lang racket
(define-for-syntax (grace-struct-syntax prefix stx) 
  (syntax-case stx ()
    [(_ (struct-name (field ...) struct-option ...) ...)
     (with-syntax ([(grace:struct ...) (map (lambda (id)
                                              (datum->syntax
                                               id
                                               (string->symbol
                                                (format "~a:~a" prefix (syntax-e id)))))
                                            (syntax->list (syntax (struct-name ...))))])
       (syntax (begin (define-struct grace:struct (field ...) struct-option ... #:transparent) ...)))]))

(define-syntax (define-grace-structs stx)
  (grace-struct-syntax "grace" stx))

(define-syntax (define-grace-types stx)
  (grace-struct-syntax "grace:type" stx))

(define-grace-structs
  (number (value))
  (str (value))
  (identifier (value type))
  
  (var-decl (name type value))
  (def-decl (name type value))
  (bind (name value))
  
  (expression (op e1 e2))
  (method-call (name args))
  (object (body))
  (method (name signature body type))
  (member (parent name))
  (return (value))
  
  (code-seq (code)))

(define number-identifier (grace:identifier "Number" #f))
(define string-identifier (grace:identifier "String" #f))
(define boolean-identifier (grace:identifier "Boolean" #f))
(define dynamic-identifier (grace:identifier "Dynamic" #f))
(define list-identifier (grace:identifier "List" #f))

(define number-other (grace:identifier "other" number-identifier))
(define boolean-othter (grace:identifier "other" boolean-identifier))
(define string-other (grace:identifier "other" string-identifier))
(define top-other (grace:identifier "other" dynamic-identifier))
(define list-other (grace:identifier "other" list-identifier))

(define-struct grace:type (methods) #:transparent)

(define-grace-types
  (method (name signature rtype))
  (string ())
  (list ())
  (boolean ())
  (dynamic ())
  (void ())
  (done ()))

(define grace:type:number (grace:type (list 
                        (grace:type:method + (list number-other) number-identifier)
                        (grace:type:method - (list number-other) number-identifier)
                        (grace:type:method * (list number-other) number-identifier)
                        (grace:type:method / (list number-other) number-identifier)
                        (grace:type:method modulo (list number-other) number-identifier)
                        (grace:type:method exp (list number-other) number-identifier)
                        
                        (grace:type:method equal? (list number-other) top-other)
                        (grace:type:method 'not (list number-other) top-other)
                        (grace:type:method < (list number-other) boolean-identifier)
                        (grace:type:method > (list number-other) boolean-identifier)
                        (grace:type:method <= (list number-other) boolean-identifier)
                        (grace:type:method >= (list number-other) boolean-identifier))))

(provide (all-defined-out))