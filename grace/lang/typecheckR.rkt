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


;; Gets the type environment and looks for type errors in the grace code.
;;
;; Params:
;;   stx - The grace code given as a list of statements.
;; Returns:
;;   Nothing for now, evaluated for side effects of signaling errors.
;;
;; TODO: Fix return type.
;(: typecheck ((Syntaxof Any) -> GraceType))
(: typecheck-body (BodyType -> GraceType))
(define (typecheck-body stx)
  
  ;; Get the type environment for the current scope by calling build-environment.
  (let-values ([(current-type-defs current-type-env)
                (build-environment stx)])
    
    ;; Push the scope environments onto the stack.
    (push current-type-defs type-defs)
    (push current-type-env type-envs)
    
    ;; Typecheck each element in the body
    (for ([elt (unwrap stx)])
      (typecheck elt))
    
    ;; TODO: Define stack of type envs in the prelude, then push here, and
    ;;   pop inner-scope ones after we return from the recursive call.
    
    ;; TODO: Fix return
    (hash-ref current-type-defs "#SelfType#")))


;; NOTES: This function dispatches all typecheck calls to smaller ones.
;; It will return the type of a statement.
;; Any BodyTypes (ie. (Syntaxof (Listof (Syntaxof Any)))) Should be directed
;; at `typecheck-body`.
(: typecheck ((Syntaxof Any) -> GraceType))
(define (typecheck stmt)
  (match (unwrap stmt)
    ((grace:type-annot value) value)
    
    ((grace:number value) "Number")
    
    ((grace:str value) "String")
    
    ((grace:identifier value type) (void))
    
    ((grace:method-def name signature rtype) (void))
    
    ((grace:type-def name methods) (void))
    
    ((grace:var-decl name type value) (void))
    
    ((grace:def-decl name type value) (void))
    
    ((grace:bind name value) (void))
    
    ((grace:expression op lhs rhs) (void))
    
    ((grace:method-call name args) (void))
    
    ((grace:object body) (void))
    
    ((grace:method name signature body rtype) (void))
    
    ((grace:member parent name) (void))
    
    ((grace:return value) (void))
    
    ((grace:if-then-else check tbody ebody) (void))
    
    ((grace:class-decl name param-name signature body) (void))
    
    ((grace:newline) (void))
    
    (else 
     (error 'Typechecker "Found unknown structure while typechecking")))
  
  (list))



;; Builds up the type environment for the outermost scope in stx.
;;
;; Params:
;;   stx - A list of grace code statements.
;; Returns:
;;   - A hash of types defined in the scope.
;;   - A hash of identifiers linked to their types.
(: build-environment (BodyType -> (values ScopeTypeDefs ScopeTypeEnv)))
(define (build-environment stx)
  ;; Create a hash of typedefs and the type-env in the current scope.
  (define: current-type-defs : ScopeTypeDefs
    (make-hash))
  (define: current-type-env : ScopeTypeEnv
    (make-hash))
  
  
  ;; Add selftype to typedefs
  (hash-set! current-type-defs "#SelfType#" (list))
  
  
  ;; Add any type defs to the hash.
  (for ([elt (unwrap stx)])
    (match (unwrap elt)
      ((grace:type-def name methods) 
       (hash-set! current-type-defs 
                  (id-name name)
                  (map get-method-def-type (unwrap methods))))
      (else 'none)))   
  
  
  ;; Add any identifiers to the type environment.
  (for ([elt (unwrap stx)])
    (match (unwrap elt)
      
      ;; TODO: For var- and def-decl, add methods to selftype?
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
       (add-method-to "#SelfType#"
                      (get-method-impl-type
                       (grace:method name signature body rtype))
                      ;(get-method-def-type 
                      ; (cast 
                      ;  (datum->syntax #f (grace:method-def name signature rtype)) 
                      ;  (Syntaxof grace:method-def)))
                      current-type-defs))
      
      ((grace:class-decl name param-name signature body)
       (let* ([name-string (id-name name)]
              [param-name-string (id-name param-name)]
              [signature-string (map id-type (unwrap signature))]
              [class-name (format "#~aClassType#" name-string)]
              [obj-name (format "~aType" name-string)]
              ;; TODO: Implement functions for this to work.
              [obj-type (obj-body-to-methods body)]
              [class-type (list (MethodType param-name-string 
                                            signature-string 
                                            obj-name))])
         
         ;; Then implement a class declaration
         ;; 
         ;; - class C.X(...) { ... }
         ;; 
         ;; as
         ;;
         ;; -  def C = object { 
         ;; -    method X(...) {
         ;; -      return object { ... }
         ;; -    }
         ;; -  }
         ;; 
         ;; A definition of an object that takes a method that returns an object.
         
         ;; First set the object type in our type defs.
         (hash-set! current-type-defs
                    obj-name
                    obj-type)
         
         ;; Then, add the class type into our type defs.
         (hash-set! current-type-defs
                    class-name
                    class-type)
         
         ;; Finally, link the name of the class to the class type as a def.
         (hash-set! current-type-env
                    name-string
                    (IDInfo class-name "def"))))
      
      (else 'none)))
  
  ;; TODO: Remove, for debugging.
  (display-type-env current-type-defs current-type-env)
  
  
  (values current-type-defs current-type-env))


;; Takes a method definition and returns a method-type as defined in ast.
(: get-method-def-type ((Syntaxof grace:method-def) -> MethodType))
(define (get-method-def-type method)
  (let* ([method (syntax-e method)]
         [name-string (id-name (grace:method-def-name method))]
         [signature-string (map id-type (unwrap (grace:method-def-signature method)))]
         [rtype-string (type-name (grace:method-def-rtype method))])
    (MethodType name-string signature-string rtype-string)))


;; Uses the above function to get the method type for a method implementation,
;; for now.
(: get-method-impl-type (grace:method -> MethodType))
(define (get-method-impl-type method)
  (let* ([name (grace:method-name method)]
         [signature (grace:method-signature method)]
         [rtype (grace:method-rtype method)])
    (get-method-def-type 
     (cast 
      (datum->syntax #f (grace:method-def name signature rtype)) 
      (Syntaxof grace:method-def)))))


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
;;(define no-type "#MissingType#")
(: type-name (TypeType -> String))
(define (type-name type)
  (let* ([type-string (grace:type-annot-value (unwrap type))])
    (if (equal? type-string "#MissingType#")
        "Dynamic*"
        type-string)))
;(define (type-name type)
;  (let* ([type-exists (unwrap type)])
;    (if type-exists
;        (id-name (cast type IdentifierType))
;        "Dynamic*")))


(: add-method-to (String MethodType ScopeTypeDefs -> Any))
(define (add-method-to type method type-defs)
  (let* ([old-methods (hash-ref type-defs type)]
         [new-methods (push method old-methods)])
    (hash-set! type-defs
               type
               new-methods)))


(: obj-body-to-methods ((Syntaxof (Listof (Syntaxof Any))) -> (Listof MethodType)))
(define (obj-body-to-methods body)
  
  (define: method-list : (Listof MethodType) (list))
  
  (for ([stmt (unwrap body)])
    (match (unwrap stmt)
      ((grace:def-decl name type value)
       (let* ([name-string (id-name name)])
         (set! method-list
               (cons (MethodType name-string
                                 (list)
                                 (type-name type))
                     method-list))))
      
      ((grace:var-decl name type value)
       (let* ([name-string (id-name name)])
         (set! method-list
               (cons (MethodType name-string
                                 (list)
                                 (type-name type))
                     method-list))
         (set! method-list
               (cons (MethodType (format "~a:=" name-string)
                                 (list (type-name type))
                                 "Done")
                     method-list))))
      
      ((grace:method name signature body rtype)
       (set! method-list
             (cons (get-method-impl-type
                    (grace:method name signature body rtype))
                   method-list)))
      
      (else 'none)))
  
  method-list)


;; Raises a typechecking Error.
(: tc-error ((Syntaxof Any) String Any * -> Any))
(define (tc-error stx msg . rest)
  (raise-syntax-error 'Typechecking (apply format msg rest) stx))



;; Entry point for typechecking.
;; TODO: Possibly fix return type.
(: typechecker ((Syntaxof grace:code-seq) -> Any))
(define (typechecker program)
  (typecheck-body (grace:code-seq-code (unwrap program))))
;(void))

(provide typechecker)









;; ##### DEBUGGING CODE #####

(: display-type-env (ScopeTypeDefs ScopeTypeEnv -> Any))
(define (display-type-env current-type-defs current-type-env)
  ;; TODO: Remove, for debugging.
  (: fill-to (Real String -> String))
  (define (fill-to num str)
    (if (>= (string-length str) num)
        str
        (string-append " " (fill-to (- num 1) str))))
  
  ;; TODO: Remove, for debugging
  (define: fill-amt : Real
    ((lambda (x)
       (apply max x))
     (cast
      (map string-length
           (append (hash-keys current-type-defs)
                   (hash-keys current-type-env)))
      (Listof Real))))
  
  
  ;; TODO: Remove, for debugging.
  (displayln "\n\n # The currently defined types are - \n")
  (for ([(key value) current-type-defs])
    (display (fill-to fill-amt key))
    (display " = " )
    (if (empty? value)
        (displayln "#s(EMPTY)")
        (begin
          (displayln (car value))
          (map (lambda (val)
                 (display (fill-to fill-amt ""))
                 (display " + " )
                 (displayln val))
               (cdr value)))))
  
  ;; TODO: Remove, for debugging.
  (displayln "\n\n # The current type environment is - \n")
  (for ([(key value) current-type-env])
    (display (fill-to fill-amt key))
    (display " = ")
    (displayln value)))
