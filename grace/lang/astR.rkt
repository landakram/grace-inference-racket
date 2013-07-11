#lang racket

(define-for-syntax (grace-struct-syntax prefix stx)
  (syntax-case stx ()
    [(_ (struct-name (field ...) struct-option ...) ...)
     (with-syntax
      ([(grace:struct ...)
        (map
         (lambda (id)
           (datum->syntax id
            (string->symbol (format "~a:~a" prefix (syntax-e id)))))
         (syntax->list (syntax (struct-name ...))))])
      (syntax
       (begin
         (define-struct
           grace:struct (field ...) struct-option ... #:prefab) ...)))]))

(define-syntax (define-grace-structs stx)
  (grace-struct-syntax "grace" stx))

(define-grace-structs
  (number (value)))
