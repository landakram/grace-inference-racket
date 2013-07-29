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

(: push-scope (ScopeTypeDefs ScopeTypeEnv -> Any))
(define (push-scope type-def type-env)
  (set! type-defs (cons type-def type-defs))
  (set! type-envs (cons type-env type-envs)))

(: pop-scope (-> Any))
(define (pop-scope)
  (set! type-defs (cdr type-defs))
  (set! type-envs (cdr type-envs)))



;; ----- TODO -----
;; Add set-type and get-type functions where set-type will trigger an
;; error if an identifier already exists in the environment. And for get-type
;; allow for two or three params, for default behavior for not found keys.
;; 
;; TIP: Use case-lambda
;; (define greet
;;   (case-lambda:
;;     [([x : Number]) ... ]
;;     [([x : Number] [y : Number]) ... ]))
;; ----------------

(define get-type
  (case-lambda:
    [([id : String] [env : ScopeTypeEnv]) 
     (hash-ref env id (lambda () #f))]
    [([id : String] [env : ScopeTypeEnv] [fn : (-> Any)])
     (hash-ref env id fn)]))

(: set-type (String IDInfo ScopeTypeEnv -> Any))
(define (set-type id type env)
  (let* ([assigned-type (get-type id env)])
    (if assigned-type 
        ;(tc-error (datum->syntax #f "")
        ;          "Identifier `~a` has already been assigned type `~a`. Cannot reassign to type `~a`."
        ;          id
        ;          assigned-type
        ;          (IDInfo-type type))
        #f
        (hash-set! env id type))))



;; PRELUDE ---------------------------------------
;; -----------------------------------------------

;; TODO: Move to different file

;; Prelude: Builtin types.
(define: prelude-type-defs : ScopeTypeDefs
  (make-hash))
(define: prelude-type-env : ScopeTypeEnv
  (make-hash))

;; TODO: Add rest of builtin types.

(hash-set!
 prelude-type-defs
 "Number"
 ;; TODO: Add other methods for numbers...
 (list
  (MethodType "plus" (list "Number") "Number")
  (MethodType "minus" (list "Number") "Number")
  (MethodType "mult" (list "Number") "Number")
  (MethodType "div" (list "Number") "Number")
  (MethodType "modulo" (list "Number") "Number")
  (MethodType "exp" (list "Number") "Number")
  
  ))

(hash-set!
 prelude-type-defs
 "String"
 ;; TODO: Add methods for strings...
 (list))

(hash-set! prelude-type-defs "Done" (list))

(hash-set! prelude-type-defs "List" (list))

(hash-set! prelude-type-defs "Boolean" (list))

(hash-set! prelude-type-defs "Dynamic" (list))
(hash-set! prelude-type-defs "Dynamic*" (hash-ref prelude-type-defs "Dynamic"))

(hash-set! prelude-type-defs "Object" (list))

(hash-set! prelude-type-defs "Top" (list))

;; Builtin methods
(hash-set!
 prelude-type-defs
 "#SelfType#"
 (list
  (MethodType "print" (list "Top") "Done")))


;; Add builtin identifiers.
(hash-set! prelude-type-env "true" (IDInfo "Boolean" "builtin"))
(hash-set! prelude-type-env "false" (IDInfo "Boolean" "builtin"))
(hash-set! prelude-type-env "self" (IDInfo "#SelfType#" "builtin"))

;; TODO: Maybe fix type of outer type to #SelfType# and figure out how to
;;   look up outer scopes in typechecking logic.
(hash-set! prelude-type-env "outer" (IDInfo "#OuterType#" "builtin"))



;; Add prelude types to the topmost scope.
(push-scope prelude-type-defs prelude-type-env)

;; END PRELUDE -----------------------------------
;; -----------------------------------------------



;; Looks for a type in the current stack of type defs.
;;
;; Params:
;;   type-string - String name of the type
;; Returns:
;;   The GraceType if found or else #f.
(: find-type (String -> (U #f GraceType)))
(define (find-type type-string)
  (define: type-found : (U #f GraceType) #f)
  
  ;; Look through the defined typedefs to find the type.
  (for ([env type-defs])
    (unless type-found
      (set! type-found
            (hash-ref env type-string (λ () #f)))))
  
  ;; Return found type or #f.
  type-found)


;; Looks for the type of an identifier in the stack of type envs.
;;
;; Params:
;;   id - The identifier to look for in string form.
;; Returns:
;;   The string form of the type if found or else "#NoTypeFound#" if not.
(: find-id (String -> String))
(define (find-id id)
  (define identifier-type "#NoTypeFound#")
  (for ([type-env type-envs])
    (when (equal? identifier-type "#NoTypeFound#")
      (set! identifier-type
            (IDInfo-type
             (hash-ref type-env
                       id
                       (λ () (IDInfo "#NoTypeFound#" "")))))))
  identifier-type)




;; Unwraps the syntax structure off of a struct.
(: unwrap (All (A) ((Syntaxof A) -> A)))
(define (unwrap elt)
  (syntax-e elt))


;; TODO: REMOVE UNLESS NEEDED
#|
(: method-equal? (MethodType MethodType -> Boolean))
(define (method-equal? m1 m2)
  (let* ([m1-name (MethodType-name m1)]
         [m2-name (MethodType-name m2)]
         [m1-signature (MethodType-signature m1)]
         [m2-signature (MethodType-signature m2)]
         [m1-rtype (MethodType-rtype m1)]
         [m2-rtype (MethodType-rtype m2)])
    (displayln (equal? m1-name m2-name))
    (displayln (equal? m1-signature m2-signature))
    (displayln (equal? m1-rtype m2-rtype))
               
    (if (and (equal? m1-name m2-name)
             (equal? m1-signature m2-signature)
             (equal? m1-rtype m2-rtype))
        #t
        #f)))
|#


;; 
(: conforms-to? (String String -> Boolean))
(define (conforms-to? conforming-type conform-to-type)
  (cond
    ((equal? conform-to-type "Dynamic") #t)
    ((equal? conform-to-type "Dynamic*") #t)
    ((equal? conforming-type "Dynamic") #t)
    ((equal? conform-to-type "Dynamic*") #t)
    ((equal? conform-to-type conforming-type) #t)
    ((equal? conform-to-type "Top") #t)
    
    ;; TODO: Subtyping.
    ;; TODO: Subtyping right now says Number is a subtype of String
    ;; because String has no methods, fix that.
    ({conforming-type . subtype-of? . conform-to-type} #t)
    
    (else #f)))


;;
(: subtype-of? (String String -> Boolean))
(define (subtype-of? maybe-subtype maybe-supertype)
  (let* ([subtype-methods (cast (find-type maybe-subtype) GraceType)]
         [suptype-methods (cast (find-type maybe-supertype) GraceType)])
    
    ;; As far as we know, it is a subtype.
    (define: is-subtype : Boolean #t)
    
    ;; Only loop if we haven't already discovered it isn't a subtype.
    (for ([super-method suptype-methods]
          #:when is-subtype)
      
      ;; We have not yet found a matching method in the maybe-subtype.
      (define: matching-method-found : Boolean #f)
      (for ([sub-method subtype-methods])
        (unless matching-method-found
          (set! matching-method-found
                (equal? sub-method super-method))))
      
      ;; If we didn't find a matching method we don't have a subtype.
      (unless matching-method-found
        (set! is-subtype #f)))
    
    is-subtype))



(: find-method-in (String String -> MethodType))
(define (find-method-in method-name where)
  (match where
    ("#SelfType#" (void))
    ("#OuterType#" (void))
    ("#All#" (void))
    (else (void)))
  
  ;; TODO: Fix this.
  (MethodType "" (list) ""))





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
    (push-scope current-type-defs current-type-env)
    
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
;(: typecheck ((Syntaxof Any) -> GraceType))
(: typecheck ((Syntaxof Any) -> String))
(define (typecheck stmt)
  (match (unwrap stmt)
    ((grace:type-annot value) value)
    
    ((grace:number value) "Number")
    
    ((grace:str value) "String")
    
    ;; For an identifier, we look for it in our stack of type environments, and
    ;; if it is not found, we trigger a typechecking error, otherwise we return
    ;; the type of the identifier as defined in the topmost scope.
    ((grace:identifier value type) 
     (let* ([identifier-type "#NoTypeFound#"]
            
            ;; Work around for `unwrap` (using `syntax-e`) nesting syntax structure.
            [value (cast (syntax->datum (cast value (Syntaxof Any))) String)])
       
       (set! identifier-type (find-id value))
       
       ;; If the identifier is not found in any of them, tc-error.
       (when (equal? identifier-type "#NoTypeFound#")
         (tc-error stmt 
                   "Identifier `~a` is not defined in this context."
                   value))
       
       ;; Return the type of the identifier.
       identifier-type))
    
    ;; Check that the types given are defined.
    ((grace:method-def name signature rtype)
     (let* ([rtype-string (type-name rtype)]
            [signature-strings (map id-type (unwrap signature))])
            
       ;; For each element in the signature, look for it in type defs.
       (for ([sigtype-string signature-strings])
         (let* ([sigtype-found (find-type sigtype-string)]) 
           
         ;; If we can't find it, tc-error.
           (unless sigtype-found
             (tc-error stmt
                       "Type `~a` is not defined in this context."
                       sigtype-string))))
       
       ;; Look for the rtype in the type defs.
       (let* ([rtype-found (find-type rtype-string)])
         
         ;; If we didn't find it, tc-error.
         (unless rtype-found
           (tc-error stmt
                     "Type `~a` is not defined in this context."
                     rtype-string)))
       
       ;; A method definition is a statement that returns Done.
       "Done"))
    
    ;; Check each of the method-defs in methods.
    ((grace:type-def name methods) 
     (begin
       ;; Typecheck each of the method definitions.
       (map typecheck (unwrap methods))
       
       ;; A type definition is a statement that returns Done.
       "Done"))
    
    
    ;; ----- TODO -----
    ;; When an object and a var or def's type do not match up, do not error
    ;; with type "#SelfType#", but rather something more useful.
    ;;  1. Possibility : Somehow get the method that could not be found in the
    ;;       value-type and pretty-print it to show what could not be found.
    ;; ----------------
    
    ;; Make sure the given type and the type of the value match.
    ((grace:var-decl name type value)
     (let* ([name-string (id-name name)]
            [type-string (type-name type)]
            [type-found (find-type type-string)]
            
            ;; TODO: This is hacky because value can be type-annot or #f.
            [value-type-string (if (unwrap value)
                                   (typecheck value)
                                   "#NoValue#")])
       
       ;; Make sure that the type annotated exists in the context.
       (unless type-found
         (tc-error stmt
                   "Type `~a` is not defined in this context."
                   type-string))
       
       ;; If a value is given in an assignment, make sure the type of the
       ;; expression matches the annotated type.
       (unless (equal? value-type-string "#NoValue#")
         (unless { value-type-string . conforms-to? . type-string }
           (if (equal? value-type-string "#SelfType#")
               (tc-error stmt
                         "Given object does not conform to type `~a`."
                         type-string)
               (tc-error stmt
                         "Given value of type `~a` while expecting value of type `~a`."
                         value-type-string
                         type-string))))
       
       ;; A variable declaration returns done.
       "Done"))
    
    ;; Make sure the given type and the type of the value match.
    ((grace:def-decl name type value) 
     (let* ([name-string (id-name name)]
            [type-string (type-name type)]
            [type-found (find-type type-string)]
            [value-type-string (typecheck value)]) 
       
       ;; Make sure that the type annotated exists in the context.
       (unless type-found
         (tc-error stmt
                   "Type `~a` is not defined in this context."
                   type-string))
       
       ;; If a value is given in an assignment, make sure the type of the
       ;; expression matches the annotated type.
       (unless { value-type-string . conforms-to? . type-string }
         (if (equal? value-type-string "#SelfType#")
             (tc-error stmt
                       "Given object does not conform to type `~a`."
                       type-string)
             (tc-error stmt
                       "Given value of type `~a` while expecting value of type `~a`."
                       value-type-string
                       type-string)))
       
       "Done"))
    
    ;; Make sure the type in the env and the type of the value match.
    ((grace:bind name value) 
     (let* ([name-string (id-name name)]
            [type-string (typecheck name)]
            [value-type-string (typecheck value)])
       (unless (equal? type-string value-type-string)
         (tc-error stmt
                   "Can not assign value of type `~a` to variable `~a` of type `~a`."
                   value-type-string
                   name-string
                   type-string))
      
       ;; A variable assignment returns done.
       "Done"))
    
    ((grace:expression op lhs rhs)
     (let* ([lh-type (typecheck lhs)]
            [rh-type (typecheck rhs)]
            [lh-methods (cast (find-type lh-type) GraceType)]
            
            ;; Work-around for sytnax-e pushing down syntax structure
            [op (cast (syntax->datum (cast op (Syntaxof Symbol))) Symbol)]
            
            ;; Look for the operator in the type of the left hand side.
            [op-found (findf (λ: ([method : MethodType])
                               (equal? (MethodType-name method) 
                                       (symbol->string op)))
                             lh-methods)])
            ;[op-found (member (symbol->string op) lh-methods)])
       
       ;; If the operator wasn't found, tc-error.  
       (unless op-found
         (tc-error stmt
                   "There is no such operator `~a` in type `~a`."
                   (symbol->string op)
                   lh-type))
       
       (let* (;; `findf` returns #f is the method isn't found, but we know it has been.
              [op-found (cast op-found MethodType)]
              [op-signature (MethodType-signature op-found)]
              [second-type (car op-signature)]
              [rtype (MethodType-rtype op-found)])
         
         ;; If the operator is found. Make sure the type of the right hand side
         ;; conforms to the expected type.
         (unless { rh-type . conforms-to? . second-type }
           (tc-error stmt
                     "The operator takes something of type `~a` but got `~a` instead."
                     second-type
                     rh-type))
         
         ;; An expression returns the return type of the operator.
         rtype)))
       
       
       ;; `findf` returns #f if the method wasn't found or the method itself if it is.
       ;(if op-found
       ;    
       ;    ;; If the operator is found. Make sure the type of the right hand side
       ;    ;; conforms to the expected type.
       ;    (let* ([op-signature (MethodType-signature op-found)]
       ;           [second-type (car op-signature)]
       ;           [rtype (MethodType-rtype op-found)])
       ;      (unless { rh-type . conforms-to? . second-type }
       ;      ;(unless (equal? second-type rh-type)
       ;        (tc-error stmt
       ;                  "The operator takes something of type `~a` but got `~a` instead."
       ;                  second-type
       ;                  rh-type))
       ;      rtype))
           
           ;; If the operator wasn't found, tc-error.
           ;(begin
           ;  (tc-error stmt
           ;            "There is no such operator `~a` in type `~a`."
           ;            (symbol->string op)
           ;            lh-type)
           ;  "#SHOULD_NEVER_SEE_THIS#"))))
           
    
    ((grace:method-call name args) 
     (match (unwrap name)
       
       ;; TODO: This might not work and we might have to recursively call typecheck, since
       ;; we have no idea what parent might be. It could be an expression, or another member
       ;; call, etc... The problem is that typecheck only returns the return type of a statement,
       ;; so when we get to typechecking grace:members, etc, we need to figure out some way to
       ;; actually get the whole method out of it so we can typecheck the signature along with
       ;; the rtype, etc.
       ;;
       ;; It might be okay to do this first check here and any recursive ones outside because
       ;; this final "name" will be the name of the method whereas any recursive ones will be
       ;; names of objects that contain another object, etc... and then typecheck for 
       ;; grace:member can simply return the name of the type of the returned objects.
       ((grace:member parent name)
        (let* ([name-string (id-name name)]
               [parent-type (typecheck parent)]
               [method-found (find-method-in name-string parent-type)])
          "#Void#"))
       
       ((grace:identifier value type) 
        (let* ([name (cast name IdentifierType)]
               [name-string (id-name name)]
               [method-found (find-method-in name-string "#All#")])
          
          (unless method-found
            (tc-error stmt
                      "No such method `~a` found."
                      name-string))
          
          (let* ([method-signature (MethodType-signature method-found)]
                 [method-rtype (MethodType-rtype method-found)]
                 
                 ;; Workaround for syntax-e pushing nested syntax structure.
                 [args (if (not (empty? args)) 
                           (cast (syntax-e (cast args (Syntaxof Any))) (Listof (Syntaxof Any)))
                           (list))]
                 
                 ;; Get the types of the expressions in the arguments.
                 [arg-types (map typecheck args)])
            
            (unless (equal? arg-types method-signature)
              (tc-error stmt
                        "Method `~a` got arguments of the wrong type.\n~aExpected: ~a\n~aArguments: ~a"
                        name-string
                        "          "
                        method-signature
                        "          "
                        arg-types))
            
            method-rtype)))))
    
    ;; For an object, typecheck its body then return the type of the object.
    ((grace:object body)
     (begin
       (typecheck-body (grace:object-body (cast (unwrap stmt) grace:object)))
       
       ;; TODO: Fix this type since the object might be popped off.
       ;;   Also, this doesn't allow for any def a : ObjectX = object { ... }
       ;;   because right now, it is comparing "ObjectX" to "#SelfType". To fix
       ;;   that, probably need to implement subtyping in `conforms-to?`.
       "#SelfType#"))
    
    
    ;; At this point, we need to look for return statements in the body. We need
    ;; to collect all of them, and make sure that they all match the rtype if given.
    ;; QUESTION: If a methods rtype is not specified, do we treat it as Dynamic*? Or
    ;; do we infer it from the return types?
    ;; Also we need to confirm all the types exist.
    ((grace:method name signature body rtype) "#Void#")
    
    ((grace:member parent name) "#Void#")
    
    ((grace:return value) "#Void#")
    
    ((grace:if-then-else check tbody ebody) "#Void#")
    
    ((grace:class-decl name param-name signature body) "#Void#")
    
    ((grace:newline) "#Void#")
    
    (else 
     (tc-error stmt "Found unknown structure while typechecking") 
     "ERROR")))
  ;(list))



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
         ;(hash-set! current-type-env 
         ;           name-string 
         ;           (IDInfo type-string "var"))
         (unless (set-type name-string (IDInfo type-string "var") current-type-env)
           (tc-error elt
                     "The identifier `~a` has already been declared and cannot be declared again."
                     name-string))
         
         (add-method-to "#SelfType#"
                        (MethodType name-string
                                    (list)
                                    type-string)
                        current-type-defs)
         (add-method-to "#SelfType#"
                        (MethodType (format "~a:=" name-string)
                                    (list type-string)
                                    "Done")
                        current-type-defs)))
      
      ((grace:def-decl name type value)
       (let* ([name-string (id-name name)]
              [type-string (type-name type)])
         ;(hash-set! current-type-env
         ;           name-string
         ;           (IDInfo type-string "def"))
         (unless (set-type name-string (IDInfo type-string "def") current-type-env)
           (tc-error elt
                     "The identifier `~a` has already been declared and cannot be declared again."
                     name-string))
         
         (add-method-to "#SelfType#"
                        (MethodType name-string
                                    (list)
                                    type-string)
                        current-type-defs)))
      
      ;; TODO: Fix this so if rtype is not given, we infer it from last statement.
      ;; Might maybe have to do this later after the environment has been built up.
      ;; So probably do this in the typecheck function.
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
  (raise-syntax-error 
   'Typechecking
   (string-append
    (format "<Line ~a>\n\n" (syntax-line stx))
    (fill-to 5 "") 
    (format "~a\n\n" (apply format msg rest)))
   
   ;; Eventually, (syntax-source stx) for filename and syntax-line for line 
   ;; number, so we can get rid of the mess above.
   stx))



;; Entry point for typechecking.
;; TODO: Possibly fix return type.
(: typechecker ((Syntaxof grace:code-seq) -> Any))
(define (typechecker program)
  (typecheck-body (grace:code-seq-code (unwrap program))))
;(void))

(provide typechecker)









;; ##### DEBUGGING CODE #####

;; TODO: Remove, for debugging.
(: fill-to (Real String -> String))
(define (fill-to num str)
  (if (>= (string-length str) num)
      str
      (string-append " " (fill-to (- num 1) str))))


(: display-type-env (ScopeTypeDefs ScopeTypeEnv -> Any))
(define (display-type-env current-type-defs current-type-env)
  
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
