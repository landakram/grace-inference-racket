#lang typed/racket

(require "ast.rkt"
         "parseR.rkt")

(struct: method-type
  ([name : String]
   [signature : (Listof String)]
   [rtype : String]))

(define-type scope-type-defs
  (HashTable String (Listof method-type)))

(define-type scope-type-env
  (HashTable String String))

(: current-type-defs scope-type-defs)
(define current-type-defs
  (make-hash))

(: current-type-env scope-type-env)
(define current-type-env
  (make-hash))

(: type-defs (Listof scope-type-defs))
(define type-defs
  (list current-type-defs))

(: type-env (Listof scope-type-env))
(define type-env
  (list current-type-env))

(: get-type-defs (Syntax -> (Listof scope-type-defs)))
(define (get-type-defs stx)
  (list))

(: build-type-env (Syntax -> (Listof scope-type-env)))
(define (build-type-env stx)
  (list))

;; TODO: Fix return type
(: typecheck (Syntax -> Any))
(define (typecheck stx)
  (let* ([type-defs (get-type-defs stx)]
         [type-env (build-type-env stx)])
    
    ;; TODO: implement
    (void)))















;; TODO: Possibly Remove

;; The syntax currently being resolved
(: stx (Parameterof Syntax))
(define stx
  (make-parameter (datum->syntax #f (list))))

;; Raise a typechecking error and formats the message.
;(define: (tc-error [msg : String] . [rest : String *]) : Any
(: tc-error (String String * -> Any))
(define (tc-error msg . rest)
  (raise-syntax-error 'typecheck (apply format msg rest) (stx)))


(struct: type-node-type
  ([name : String]
   [methods : (Listof method-type)]
   [subtype : (type-node-type -> Boolean)]))
