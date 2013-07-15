#lang typed/racket

(require "astR.rkt")


;; The list of methods that defines a type in grace.
(define-type grace-type
  (Listof method-type))

;; In each scope, we have a map of type names to type defs.
(define-type scope-type-defs
  (HashTable String (Listof grace-type)))


;; A struct containing information about an identifier.
;;
;; Fields
;;   name - identifier name
;;   type - name of the type of the identifier, "" if missing.
;;   kind - 'var' or 'def'
(struct: identifier-info
  ([name : String]
   [type : String]
   [kind : String]))

;; In each scope, we have a map of identifiers to their type names.
(define-type scope-type-env
  (HashTable String identifier-info))


;; Contains the stack of hashes of the type environments and type definitions.
(define: type-defs : (Listof scope-type-defs)
  (list))

(define: type-envs : (Listof scope-type-env)
  (list))


;; Push and pop methods for the stack. Returns the modified list.
(: push (Any (Listof Any) -> (Listof Any))) 
(define (push item lst)
  (cons item lst))

(: pop ((Listof Any) -> (Listof Any)))
(define (pop lst)
  (cdr lst))


;; Does all the logic.
;; TODO: Fix return type.
;(: typecheck ((Syntaxof grace:code-seq) -> Any))
(: typecheck ((Listof (Syntaxof Any)) -> Any))
(define (typecheck stx)
  
  ;; Create a hash of typedefs and the type-env in the current scope.
  (define: current-type-defs : scope-type-defs
    (make-hash))
  (define: current-type-env : scope-type-env
    (make-hash))
  
  ;; TODO: Define stack of type envs in the prelude, then push here, and
  ;;   pop inner-scope ones after we return from the recursive call.
  
  ;; TODO: Fix return
  (void))


;; Entry point for typechecking.
;; TODO: Possibly fix return type.
(: typechecker ((Syntaxof grace:code-seq) -> Any))
(define (typechecker program)
  (typecheck (grace:code-seq-code (syntax-e program))))

(provide typechecker)
