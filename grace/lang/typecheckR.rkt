#lang typed/racket

(require "astR.rkt")


;; NOTE: TODO ...
;; Right now, certain things are not put in the ast as syntax objects by the parser.
;; These are mostly lists, such as method signature list, method lists, etc... We 
;; should change these so that they are since otherwise issues arise when syntax-e 
;; is called and pushes down syntax structure onto previously non-syntax objects. The
;; other unwrapping function, syntax->datum, does worse and removes all nested syntax
;; structure, and again type issues arise because we are change the type of something
;; from (Syntaxof a) to a or vice versa and typed/racket runs into all sorts of issues.
;;
;; Things to do:
;;   1. Go into ast.rkt and make sure everything is some (Syntaxof a)
;;   2. Change all calls of syntax->datum to syntax-e.
;;   3. In the parser, find all places where lists are inserted and change them to
;;      include an (at-src ...) call.
;;   4. Look through the test_class_example_macro_stepper.rkt file to find any other
;;      places where the syntax structure might be left out and add it in the parser.






;; The list of methods that defines a type in grace.
(define-type grace-type
  (Listof method-type))

;; In each scope, we have a map of type names to type defs.
(define-type scope-type-defs
  (HashTable String grace-type))


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


;; Unwraps the syntax structure off of a struct.
(: unwrap (All (A) ((Syntaxof A) -> A)))
(define (unwrap elt)
  (syntax-e elt))


;; Does all the logic.
;; TODO: Fix return type.
;(: typecheck ((Syntaxof grace:code-seq) -> Any))
(: typecheck ((Syntaxof (Listof (Syntaxof Any))) -> Any))
(define (typecheck stx)
  
  ;; Create a hash of typedefs and the type-env in the current scope.
  (define: current-type-defs : scope-type-defs
    (make-hash))
  (define: current-type-env : scope-type-env
    (make-hash))
  
  
  ;; Add any type defs to the hash.
  (for ([elt (unwrap stx)])
    (match (unwrap elt)
      ((grace:type-def name methods) 
       (hash-set! current-type-defs 
                  (id-name name)
                  (map get-method-type (unwrap methods))))
      (else 'none)))
  (displayln current-type-defs)
  
  ;(for-each (lambda: ([elt : (Syntaxof Any)])
  ;            (match (syntax->datum elt)
  ;              ((grace:type-def name methods)
  ;               (hash-set! current-type-defs
  ;                          (id-name name)
  ;                          (map get-method-type methods)))
  ;              (else 'none)))
  ;          stx)
  ;(displayln current-type-defs)
  
  ;; TODO: Define stack of type envs in the prelude, then push here, and
  ;;   pop inner-scope ones after we return from the recursive call.
  
  ;; TODO: Fix return
  (void))


;; Takes a method definition and returns a method-type as defined in ast.
(: get-method-type ((Syntaxof grace:method-def) -> method-type))
(define (get-method-type method)
  (let* ([method (syntax-e method)]
         [name-string (id-name (grace:method-def-name method))]
         [signature-string (map id-type (unwrap (grace:method-def-signature method)))]
         [rtype-string (id-name (grace:method-def-rtype method))])
    (method-type name-string signature-string rtype-string)))


;; Grabs the name of an identifier wrapped in a syntax object.
;;
;; Params:
;;   id - A grace:identifier struct.
;; Returns:
;;   A string of the name of the identifier.
;;   
;; NOTE: 'id' here is not unwrapped like others because unwrap pushes the syntax
;;   structure down onto grace:identifier-value and the function actually returns 
;;   (Syntaxof String), even though the typechecker thinks it returns String.
(: id-name ((Syntaxof grace:identifier) -> String))
(define (id-name id)
  (grace:identifier-value (cast (syntax->datum id) grace:identifier)))
;;  (grace:identifier-value (unwrap id)))


;; Gives the string form of the type of an identifier.
;;
;; Params:
;;   id - A grace:identifier struct.
;; Returns:
;;   A string of the type of the identifier, will return "Dynamic*"
;;   (representing missing type information) if no type was given.
(: id-type ((Syntaxof grace:identifier) -> String))
(define (id-type id)
  (let* ([type (grace:identifier-type (unwrap id))])
    (if type
        (id-name type)
        "Dynamic*")))


;; Entry point for typechecking.
;; TODO: Possibly fix return type.
(: typechecker ((Syntaxof grace:code-seq) -> Any))
(define (typechecker program)
  (typecheck (grace:code-seq-code (unwrap program))))
  ;(void))

(provide typechecker)
