#lang racket
(provide all-from-out)

(define-syntax (define-grace-structs stx)
  (syntax-case stx ()
    [(_ (struct-name (field ...)) ...)
     (with-syntax ([(grace:struct ...) (map (lambda (id)
                                              (datum->syntax
                                               id
                                               (string->symbol
                                                (format "grace:~a" (syntax-e id)))))
                                            (syntax->list (syntax (struct-name ...))))])
       (syntax (begin (define-struct grace:struct (field ...)) ...
                      (provide (struct-out grace:struct) ...))))]))

(define-grace-structs
  (var-decl (name value))
  (def-decl (name value))
  
  (bind (name value))
  
  (num-exp (n))
  (variable (i))
  
  (arith-exp (op e1 e2))
  (method-call (name args))
  (object (body))
  (method (name body))
  (member (parent name))
  
  (code-seq (code))
  (str (s)))
