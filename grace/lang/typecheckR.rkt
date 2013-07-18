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
(define-type GraceType
  (Listof MethodType))

;; In each scope, we have a map of type names to type defs.
(define-type ScopeTypeDefs
  (HashTable String GraceType))


;; A struct containing information about an identifier.
;;
;; Fields
;;   name - identifier name
;;   type - name of the type of the identifier, "" if missing.
;;   kind - 'var' or 'def'
(struct: IDInfo
  (;[name : String]
   [type : String]
   [kind : String])
  #:prefab)

;; In each scope, we have a map of identifiers to their type names.
(define-type ScopeTypeEnv
  (HashTable String IDInfo))


;; Contains the stack of hashes of the type environments and type definitions.
(define: type-defs : (Listof ScopeTypeDefs)
  (list))

(define: type-envs : (Listof ScopeTypeEnv)
  (list))


;; Push and pop methods for the stack. Returns the modified list.
(: push (All (A) (A (Listof A) -> (Listof A))) )
(define (push item lst)
  (cons item lst))

(: pop (All (A) ((Listof A) -> (Listof A))))
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
  (define: current-type-defs : ScopeTypeDefs
    (make-hash))
  (define: current-type-env : ScopeTypeEnv
    (make-hash))
  
  
  ;; Add selftype to typedefs
  (hash-set! current-type-defs "self" (list))
  
  
  ;; Add any type defs to the hash.
  (for ([elt (unwrap stx)])
    (match (unwrap elt)
      ((grace:type-def name methods) 
       (hash-set! current-type-defs 
                  (id-name name)
                  (map get-method-type (unwrap methods))))
      (else 'none)))
  
  ;; TODO: Remove, for debugging.
  (displayln "\n\n # The currently defined types are - \n")
  (for ([(key value) current-type-defs])
    (display key)
    (display ":" )
    (displayln value))   
  
  
  ;; Add any identifiers to the type environment.
  (for ([elt (unwrap stx)])
    (match (unwrap elt)
      ((grace:var-decl name type value)
       (let* ([name-string (id-name name)]
              [type-string (type-name type)])
         (hash-set! current-type-env 
                    name-string 
                    (IDInfo type-string "var"))))
      
      ((grace:def-decl name type value)
       (let* ([name-string (id-name name)]
              [type-string (type-name type)])
         (hash-set! current-type-env
                    name-string
                    (IDInfo type-string "def"))))
      
      ((grace:method name signature body rtype)
       (void))
      
      ((grace:class-decl name param-name signature body)
       (let* ([name-string (id-name name)]
              [class-name (format "Class_~a" name-string)]
              [obj-name (format "~aType" name-string)]
              ;; TODO: Implement functions for this to work.
              ;[obj-type (get-methods-from-obj-body body)]
              ;[class-type (list (MethodType param-name signature obj-type))]
              )
         
         ;; Then implement the class as a 
         ;;   def C = object { 
         ;;     method X(...) {
         ;;       return object { ... }
         ;;     }
         ;;   }
         ;; A definition of an object that takes a method that returns an object.
         (void)))
      
      (else 'none)))
  
  ;; TODO: Remove, for debugging.
  (displayln "\n\n # The current type environment is - \n")
  (for ([(key value) current-type-env])
    (display key)
    (display " : ")
    (displayln value))

  
  ;; TODO: Define stack of type envs in the prelude, then push here, and
  ;;   pop inner-scope ones after we return from the recursive call.
  
  ;; TODO: Fix return
  (void))


;; Takes a method definition and returns a method-type as defined in ast.
(: get-method-type ((Syntaxof grace:method-def) -> MethodType))
(define (get-method-type method)
  (let* ([method (syntax-e method)]
         [name-string (id-name (grace:method-def-name method))]
         [signature-string (map id-type (unwrap (grace:method-def-signature method)))]
         [rtype-string (type-name (grace:method-def-rtype method))])
    (MethodType name-string signature-string rtype-string)))


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
(: id-name (IdentifierType -> String))
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
(: id-type (IdentifierType -> String))
(define (id-type id)
  (let* ([type (grace:identifier-type (unwrap id))])
    (if type
        (type-name type)
        "Dynamic*")))


;; TODO: Remove.
;;(define no-type "__NO_TYPE_INFO")
(: type-name (TypeType -> String))
(define (type-name type)
  (let* ([type-string (grace:type-annot-value (unwrap type))])
    (if (equal? type-string "__NO_TYPE_INFO")
        "Dynamic*"
        type-string)))
;(define (type-name type)
;  (let* ([type-exists (unwrap type)])
;    (if type-exists
;        (id-name (cast type IdentifierType))
;        "Dynamic*")))


;; Entry point for typechecking.
;; TODO: Possibly fix return type.
(: typechecker ((Syntaxof grace:code-seq) -> Any))
(define (typechecker program)
  (typecheck (grace:code-seq-code (unwrap program))))
  ;(void))

(provide typechecker)
