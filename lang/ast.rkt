#lang racket
(provide all-defined-out)

(define-for-syntax (grace-struct-syntax prefix stx) 
  (syntax-case stx ()
    [(_ (struct-name (field ...)) ...)
     (with-syntax ([(grace:struct ...) (map (lambda (id)
                                              (datum->syntax
                                               id
                                               (string->symbol
                                                (format "~a:~a" prefix (syntax-e id)))))
                                            (syntax->list (syntax (struct-name ...))))])
       (syntax (begin (define-struct grace:struct (field ...) #:transparent) ...
                      (provide (struct-out grace:struct) ...))))]))

(define-syntax (define-grace-structs stx)
  (grace-struct-syntax "grace" stx))

(define-syntax (define-grace-types stx)
  (grace-struct-syntax "grace:type" stx))

(define-grace-structs
  (var-decl (name type value))
  (def-decl (name type value))
  
  (bind (name value))
  
  (number (value))
  (identifier (value type))
  
  (expression (op e1 e2))
  (method-call (name args))
  (object (body))
  (method (name signature body type))
  (member (parent name))
  (return (value))
  
  (code-seq (code))
  (str (value)))

(define-grace-types
  (number ())
  (string ())
  (list ())
  (boolean ())
  (dynamic ())
  (void ())
  (done ()))
  

