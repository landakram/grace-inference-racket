#lang racket

(require "ast.rkt"
         "parse.rkt")


;; @@@@@ PARAMETERS DEFINED HERE @@@@@

;; The environment to hold identifiers and their types.
(define env (make-parameter (make-hash)))

;; Type for the self keyword.
(define selftype (make-parameter (new grace:type:module%)))

;; Keeps track of whether the typechecking is in the scope of an object
(define in-object? (make-parameter #f))

;; The syntax currently being resolved.
(define stx (make-parameter #f))

;; The return type of the method whose scope we are in.
(define current-return-type (make-parameter #f))


;; @@@@@ FUNCTION DEFINED BELOW @@@@@

;; Raises a typechecking error and formats the message properly.
(define (tc-error msg . rest)
  (raise-syntax-error 'typecheck (apply format msg rest) (stx)))


;; Gets the type of an identifier from the environment. If no type is present,
;; returns false.
(define (get-type k)
  (hash-ref (env) k #f))


;; Sets the type for an identifier in the environment.
(define (set-type k v)
  (hash-set! (env) k v))


;; Returns whether a syntax element is an object.
(define (is-object? elt)
  (is-a? (unwrap elt) grace:type:object%))


;; Finds a method in an the object type of a parent and returns it. Returns #t
;; if the parent type is dynamic.
(define (find-method-in name parent)
  (let* ([name-string name]
         [parent (unwrap parent)])
    ; Fix string/symbol issue with name.
    (when (symbol? name)
      (set! name-string (symbol->string name)))
    
    (define parent-string "")
    
    (if (grace:identifier? parent)
        (set! parent-string
              (unwrap (grace:identifier-value parent)))
        (set! parent-string
              "literal"))
    
    (displayln "PARENT HERE:")
    (displayln parent-string)
    
    (if (check-if-dynamic parent)
        #t
        (let* ([method 
                ; Find a method that matches the name given.
                (findf 
                 (λ (a) 
                   (let* ([temp (get-field name a)])
                     ; Fix for when the method name was given as symbol.
                     (when (symbol? temp)
                       (set! temp (symbol->string temp)))
                     (equal? temp name-string)))
                 
                 ; Check user-defined and builtin methods.
                 (append (get-field builtins (expression-type parent))
                         (get-field methods (expression-type parent))))])
          
          ; If the method was not found and the parent name was missing,
          ; check the parent of the parent, and so on... 
          (if (and (not method)
                   (equal? parent-string "missing"))
              
              (let* ([parent-parent (get-field parent parent)])
                (if parent-parent
                    (find-method-in name parent-parent)
                    #f))
              
              method)))))


;; Checks whether the object type of an identifier is dynamic.
(define (check-if-dynamic obj)
  ; (displayln "HERE")
  ; (displayln obj)
  (eq? (send (expression-type obj) readable-name)
       "Dynamic"))


;; Inserts an implicit self in a method to give it a parent if it doesn't
;; have one.
;;
;; @@@@ TODO: Instead of self, insert something to indicate 'missing' so
;; we can later check self, outer, etc... Then, once we are keeping track
;; of parents and outers, we can keep looking at outers to find methods.
(define (insert-implicit-self method-name)
  (if (grace:member? method-name)
      method-name
      (grace:member
       (grace:identifier (datum->syntax (stx) "self" (stx)) #f)
       method-name)))


;; Unwraps a list of possible stx objects into a list.
(define (unwrap-list possible-stx-obj)
  (if (syntax? possible-stx-obj)
      (syntax->list possible-stx-obj)
      possible-stx-obj))


;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @@@@@ MAIN METHOD FOR TYPECHECK ENTRY @@@@@
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;; Typechecks a grace program passed in as a grace:code-seq.
(define (typecheck prog)
  (parameterize ([env (make-hash)]
                 [stx prog])
    ; Set common types in the environment.
    (set-type "Number"  (new grace:type:number%))
    (set-type "String"  (new grace:type:string%))
    (set-type "List"    (new grace:type:list%))
    (set-type "Boolean" (new grace:type:boolean%))
    (set-type "Dynamic" (new grace:type:dynamic%))
    (set-type "Done"    (new grace:type:done%))
    (set-type "Object"  (new grace:type:object% [internal-name "Object"]))
    (set-type "true"    (new grace:type:boolean%))
    (set-type "false"   (new grace:type:boolean%))
    (set-type "Top"     (new grace:type:top%))
    (set-type "self"    (selftype))

    ; Resolve types in the program.
    (resolve-identifiers-list
     (syntax->list (grace:code-seq-code (syntax-e prog))))))


;; Resolve identifiers in a code sequence.
(define (resolve-identifiers-list lst)
  (parameterize ([env (hash-copy (env))])
    (map maybe-bind-name lst)
    (map resolve-identifiers lst)
    (map expression-type lst)))


;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @@@@@ DEFINED: maybe-bind-name AND HELPERS @@@@@
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;; Binds identifiers to the type environment for custom types, var, def,
;; method declarations. Also adds methods to the self object when inside an
;; object declaration.
;;
;; Things returned from this function are meaningless. It is evaluated for its
;; side effects.
(define (maybe-bind-name elt)
  ; Recursively call maybe-bind-name to embedded syntax elements.
  (if (syntax? elt)
      (parameterize ((stx elt))
        (maybe-bind-name (syntax-e elt)))
      (cond
        ; If the element is an object, add it's type to the environment here.
        ((is-object? elt)
         (set-type (get-field internal-name (unwrap elt)) (unwrap elt)))

        ; Else, only bind if we have a variable, method, or def declaration.
        (else
         (match elt
           ((grace:var-decl name type value)
            (add-var name type value))

           ((grace:def-decl name type value)
            (add-def name type value))

           ((grace:method name signature body rtype)
            (add-method name signature body rtype))

           ((grace:class-decl name param-name signature body)
            (add-class name param-name signature body))
           ;; TODO: TYPE actually needs to be the name of a type in the
           ;; environment, so here, we need to set the type in the environment
           ;; so add-var and eventually, resolve-identifier can find it.

           (else 'success))))))


;; Adds a variable to the environment with given value.
(define (add-var name type value)
  (let* ([name-string (grace:identifier-value (syntax->datum name))]
         [type-type   (resolve-identifier type)])
    ; If we are in the scope of an object, add getter and setter.
    ; @@@@@ TODO: Only add setter if var is public @@@@@
    ; TODO self.x in outermost
    (when (in-object?)
      ; Getter.
      (add-method-to-selftype
       (new grace:type:method%
            [name name-string]
            [signature (list)]
            [rtype type-type]))

      ; Setter.
      (add-method-to-selftype
       (new grace:type:method%
            [name (string->symbol (format "~a:=" name-string))]
            [signature (list (same-other (resolve-identifier type)))]
            [rtype (get-type "Done")])))

    ; Set the type of the variable in the environment
    (set-type name-string type-type)))


;; Adds a definition to the environment with given value.
(define (add-def name type value)
  (let* ([name-string (grace:identifier-value (syntax->datum name))]
         [type-type (resolve-identifier type)])

    ; Add a getter if in the scope of an object.
    ; @@@@@ TODO: Similar to above 'var', only add getter if public. @@@@@
    (when (in-object?)
      (begin
        (add-method-to-selftype
         (new grace:type:method%
              [name name-string]
              [signature (list)]
              [rtype type-type]))))

    ; Set the type of the constant in the environment
    (set-type name-string type-type)))


;; Adds method to the environment and to self if inside an object declaration.
(define (add-method name signature body rtype)
  (let* ([type-type       (resolve-identifier rtype)]
         [method-name     (grace:identifier-value (syntax->datum name))]
         [new-method-type (new grace:type:method%
                               (name method-name)
                               (signature (syntax->datum signature))
                               (rtype type-type))])
    (add-method-to-selftype new-method-type)
    ; TODO REMOVE
    ; (displayln (send new-method-type readable-signature))
    (set-type (grace:identifier-value (unwrap name)) type-type)))


;; Adds method to the selftype if inside object declaration.
(define (add-method-to-selftype method)
  (set-field! methods (selftype) (cons method (get-field methods (selftype)))))


;; Adds a class to the environment as an object type with a method called 'new'
;; that returns an object as defined in the body.
(define (add-class name param-name signature body)
  (let* (;[name (grace:identifier-value (unwrap name))]
         [name-string (grace:identifier-value (unwrap name))]
         [class-name (format "Class_~a" name-string)]
         ;[name-string (grace:identifier-value (unwrap name))]
         [obj-name (format "~aType" name-string)]
         [obj-methods (foldl
                       body-stmt-to-method-type
                       (list)
                       (unwrap-list body))]
         [obj-type (new grace:type:object%
                        [internal-name obj-name]
                        [methods obj-methods])]
         [class-type
          (new grace:type:object%
               [internal-name class-name]
               [methods
                (list (new grace:type:method%
                           [name (grace:identifier-value (unwrap param-name))]
                           [signature (unwrap signature)]
                           [rtype obj-type]))])])
    (displayln (unwrap name))
    ; (set-type obj-name obj-type)
    (set-type class-name class-type)
    ; FIXME
    ;(set-type (grace:identifier-value (unwrap name)) class-type)
    (add-var name (grace:identifier class-name class-type) class-type)))


;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @@@@@ DEFINED: resolve-identifiers AND HELPERS @@@@@
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;; Ensures that identifiers are present in the type environment, and are used
;; consistently.
;;
;; Things returned from this function are meaningless. It is evaluated for its
;; side effects, which are to throw type errors if types are inconsistent or
;; to add things to the type environment.
(define (resolve-identifiers elt)
  (if (syntax? elt)
      ; If the element is a syntax object, unwraps it to get the nested datum
      ; structure and makes a recursive call.
      (parameterize ([stx elt])
        (resolve-identifiers (syntax-e elt)))

      ; If the element is already a datum structure, match it.
      (match elt
        ; This call just returns the type of an identifier.
        ((grace:identifier value type-identifier)
         (resolve-identifier (grace:identifier value type-identifier)))

        ; Recursively resolves identifiers for any nested expressions.
        ((grace:expression op e1 e2)
         (begin (resolve-identifiers e1)
                (resolve-identifiers e2)))

        ; Recursively resovles identifiers of the arguments & the method name.
        ((grace:method-call name args)
         (begin (resolve-identifiers name)
                (resolve-identifiers-list (syntax->list args))))

        ; Recursively resolves identifiers of the parent in a member access.
        ((grace:member parent name)
         (begin (resolve-identifiers parent)))

        ; For an if-then clause, resolves identifiers in the condition, then
        ; does so for the entire body statement.
        ((grace:if-then-else condition tbody ebody)
         (begin (resolve-identifiers condition)
                (resolve-identifiers-list (syntax->list tbody))
                (if (list? (syntax->list ebody))
                (resolve-identifiers-list (syntax->list ebody))
                ebody)))

        ; Calls a helper that looks for type errors in a method declaration.
        ((grace:method method-name signature body rtype)
         (resolve-method method-name signature body rtype))

        ; Calls a helper for object declaration that recursively resolves
        ; identifiers in the body of the declaration.
        ((grace:object body)
         (resolve-object body))

        ; For a binding, resolve identifiers for the value then call a helper
        ; to look for type errors in binding the value to the identifier.
        ((grace:bind name value)
         (resolve-identifiers value)
         (resolve-binding name value))

        ; For a variable declaration, check the declared type, if any, against
        ; the type of the expression given by value.
        ((grace:var-decl name type value)
         (resolve-declaration "var " name type value))

        ; For a constant declaration, check the declared type, if any, against
        ; the type of the expression given by value.
        ; @@@@@
        ; TODO: Make sure this and the var-decl work properly and clean it up.
        ; @@@@@
        ((grace:def-decl name type value)
         (resolve-declaration "def " name type value))

        ; For a return statement, check its type against specified return type.
        ((grace:return value)
         (resolve-return value))

        (else elt))))


;; Gets the type of an identifier in the environment by calling get-type.
;; Returns missing if the identifier is nil.
(define (resolve-identifier ident)
;  (displayln "\n IDENT:")
;  (displayln ident)
;  (displayln (unwrap ident))
  (if (false? (unwrap ident))
      ;'missing ;FIXME
      (new grace:type:dynamic*%)
      (begin
      ;(displayln "\n HERE")
      ;(displayln (unwrap ident))
      ;(print (grace:identifier-value (unwrap ident))) (display "\n")
      ;(displayln (get-type (grace:identifier-value (unwrap ident))))
      (get-type (grace:identifier-value (unwrap ident))))))


;; Resolve the identifiers in a method call.
(define (resolve-method method-name signature body rtype)
  (parameterize ([env (hash-copy (env))])
    ; Set the type of each parameter in the env.
    (for ([param (syntax->list signature)])
      (resolve-identifier param)
      (set-type (grace:identifier-value (syntax->datum param))
                (resolve-identifier
                 (grace:identifier-type (syntax->datum param)))))

    ; Check the return type of the method.
    (parameterize ([current-return-type (resolve-identifier rtype)])
      ; Error for an invalid return type.
      (when (false? (current-return-type))
        (tc-error "Return type of method not defined as a type."))

      ; Error for incorrent return type.
      (let* ([body-stmt-types (resolve-identifiers-list (syntax->list body))]
             [last-statement (last (syntax->datum body))]
             ;[real-type (expression-type last-statement)])
             [real-type (last body-stmt-types)])
        (when (and (not (grace:return? last-statement))
                   (not (conforms-to? real-type (current-return-type))))
          (displayln "\n Last-statement:")
          (displayln last-statement)
          (tc-error "Returning type ~a from method of return type ~a."
                    (send real-type readable-name)
                    (send (current-return-type) readable-name)))))))


;; Resolve the identifiers in an object declaration.
(define (resolve-object body)
  (parameterize* ([selftype (new grace:type:object% [internal-name "self"])]
                  [env (hash-copy (env))]
                  [in-object? #t])
    (set-type "self" (selftype))
    (resolve-identifiers-list (syntax->list body))))


;; Resolve the identifiers in a binding.
(define (resolve-binding name value)
  (let* ([name-type (expression-type name)]
         [value-type (expression-type value)])
    (cond
      ; The variable to be set is a simple identifier.
      ((grace:identifier? (unwrap name))
       (let* ([name-string (grace:identifier-value (unwrap name))])
         (cond
           ; We can't bind a value to a method.
           ((is-a? name-type grace:type:method%)
            (tc-error "assignment to method ~a" name-string))

           ; The identifier was undeclared.
           ((false? name-type)
            (tc-error "assignment to undeclared ~a" name-string))

           ; The types of the assignment don't match.
           ((not (conforms-to? value-type name-type))
            (tc-error
             "assigning value of nonconforming type ~a to var of type ~a"
             (send value-type readable-name)
             (send name-type readable-name)))

           (else 'success))))

      ; The variable to be set is a member of an object.
      ((grace:member? (unwrap name))
       ; Look for a setter method for the variable in the parent.
       (let* ([member-op (find-method-in
                          (format "~a:=" (grace:identifier-value
                                          (grace:member-name (unwrap name))))
                          (grace:member-parent (unwrap name)))])
         ; Ensures that we are working on a mutable variable.
         (if member-op
             ; If the assignment types do not conform, error.
             (when (not (conforms-to? value-type name-type))
               (tc-error
                "assigning value of nonconforming type ~a to var of type ~a"
                (send value-type readable-name)
                (send name-type readable-name)))

             ; If member-op was false and there was no such member in parent.
             (tc-error "no such member"))))

      (else 'success))))


;; Resolve the identifiers in a variable declaration.
(define (resolve-declaration decl-type name type value)
  (let* (;[name-dat (if (syntax? name) (syntax->datum name) (name))]
         [name-string (grace:identifier-value (unwrap name))]
         [name-type (resolve-identifier (unwrap name))]
         [_ (resolve-identifiers value)]
         [value-type (expression-type (unwrap value))]
         [type-type (resolve-identifier (unwrap type))]
         [start (syntax-position (stx))]
         [end (+ start (string-length decl-type) (syntax-span name))])
    (inference-hook start end name-string type-type value-type 'var)
    
    ;; Check if annotated type exists.
    (when (not type-type)
        (tc-error "No such type ~a exists"
                  type-type))
        
    ;; Error if the declared type and the type of the value are not the same.
    (when (not (conforms-to? value-type type-type))
      (tc-error "initializing ~a of type ~a with expression of type ~a"
                decl-type
                (send type-type readable-name)
                (send value-type readable-name)))))


;; Resolve the identifier in a return statement by making sure it matches up
;; with the return type declared by the method.
(define (resolve-return value)
  (let* ([_ (resolve-identifiers value)]
         [value-type (expression-type value)])
    (cond
      ; If current-return-type is not specified in the scope, the return
      ; statement is not inside of a method, so error.
      ((false? (current-return-type))
       (tc-error "return statement with no surrounding method"))

      ; If the return type given does not match with the return type specified
      ; by the surrouding method, error.
      ((not (conforms-to? value-type (current-return-type)))
       (tc-error "returning type ~a from method of return type ~a"
                 (send value-type readable-name)
                 (send (current-return-type) readable-name)))

      ; Return types match up, and success.
      (else 'success))))


;; Returns true if the two types conform, meaning one is dynamic, or that
;; 'conforming-type' is of the same type or a subtype of 'type'.
(define (conforms-to? conforming-type type)
  (let* ([dynamic-type (new grace:type:dynamic%)]
         [missing-type (new grace:type:dynamic*%)]
         [top-type (new grace:type:top%)]) ;FIXME
    (cond
      ((equal? type dynamic-type) #t)
      ((equal? type missing-type) #t)
      ((equal? conforming-type dynamic-type) #t)
      ((equal? conforming-type missing-type) #t)
      ((equal? conforming-type type) #t)
      ((equal? type top-type) #t)
      
      ; @@@@ NOTE: Unsure whether every type should conform to done @@@@

      ; @@@@@ TODO: Subtyping, etc. @@@@@

      ; @@@@@ TODO: Remove these, should never come up. @@@@@
      ;((equal? type 'missing)
      ; (tc-error "Type 'missing given for type in conforms-to?"))
      ;((equal? conforming-type 'missing)
      ; (tc-error "Type 'missing given for conforming-type in conforms-to?"))

      (else #f))))


;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @@@@@ DEFINED: expression-type AND HELPERS @@@@@
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;; Expression-type-helper usually gives type of expression properly.
;; Occasionally instead of giving the desired type "grace:type:..."  it gave
;; (grace:identifier "Number" #f) or something similar.  This checks if we got
;; an identifier, it searches for that string, and returns the proper type
;; with all associated methods.
(define (expression-type elt)
  (let* ((first-type (expression-type-helper elt)))
    (match first-type
      ((grace:identifier str bool)
       (get-type str))
      (else first-type))))


;; Returns the actual type of a grace expression, or Dynamic for anything else.
;; Takes in a grace-struct and outputs (grace:type:....%)
(define (expression-type-helper elt)
  (if (syntax? elt)
      ; If the element is a syntax object, unwraps it to get the nested datum
      ; structure and makes a recursive call.
      (parameterize ([stx elt])
        (expression-type-helper (syntax-e elt)))

      ; Else match it on its type and return or unwrap and return.
      (match elt
        ((grace:number value)
         (new grace:type:number%))

        ((grace:str value)
         (new grace:type:string%))

        ; For an identifier, find its type in our environment.
        ((grace:identifier value type-identifier)
         (resolve-identifier
          (grace:identifier (unwrap value) type-identifier)))

        ; For an expression, call a helper.
        ((grace:expression op e1 e2)
         (get-type-of-expression op e1 e2))

        ; For an if-then statement, make sure the condition is a boolean.
        ((grace:if-then-else condition tbody ebody)
         (let* ([cond-type (expression-type condition)])
           ;FIXME (displayln condition)
           (unless (conforms-to? cond-type (new grace:type:boolean%))
             (tc-error "if-then-else takes boolean but got ~a"
                       (send cond-type readable-name)))))

        ; For a member access, call a helper.
        ((grace:member parent name)
         (get-type-of-member parent name))

        ; For a method call, call a helper.
        ((grace:method-call name args)
         (get-type-of-method-call name args))

        ; For an object, call a helper that returns a formatted object name.
        ((grace:object body)
         (get-type-of-object body))
        
        
        ((grace:var-decl name type value)
         (new grace:type:done%))

        ; For anything else, return a dynamic type.
        (else (new grace:type:dynamic%)))))


;; Returns the type of an expression or triggers errors for type errors.
(define (get-type-of-expression op e1 e2)
  (if (check-if-dynamic e1)
      ; If e1 is dynamic, return a dynamic type.
      (begin
        (expression-type e2)
        (new grace:type:dynamic%))

      ; Else, operations are methods so look for it, then check the type
      ; of the parameter.
      (let* ([e1-type (expression-type e1)]
             [e2-type (expression-type e2)]
             [op (unwrap op)]
             [method (find-method-in op e1)])
        (cond
          (method
           ; If the method was found, get the type of its first parameter.
           (let* ([param-type (get-type
                               (grace:identifier-value
                                (grace:identifier-type
                                 (car (get-field signature method)))))])
             ; Make sure e2 conforms to the type it needs to be.
             (if (conforms-to? e2-type param-type)
                 (get-type (grace:identifier-value (get-field rtype method)))

                 ; If they don't conform, send an error.
                 (tc-error "~a takes ~a but got ~a"
                           op
                           (send param-type readable-name)
                           (send e2-type readable-name)))))

          ; If the method was not found, return an error.
          (else (tc-error "no such operator ~a in ~a"
                          op
                          (send e1-type readable-name)))))))


;; Returns the return type of a member access. Returns dynamic if the parent
;; is dynamic to accept any member access.
(define (get-type-of-member parent name)
  (let* ([parent-type (expression-type parent)]
         [name-string (unwrap (grace:identifier-value (unwrap name)))]
         [member-op (find-method-in name-string parent)])
    (if (check-if-dynamic parent)
        ; Returns dynamic if the parent is dynamic.
        parent-type
        (if member-op
            ; Get the return type of the method if it exists.
            (get-field rtype member-op)

            ; Send an error if the member was not found in the parent.
            (tc-error "no such member ~a in ~a"
                      name-string
                      (send parent-type readable-name))))))


;; Returns the return type of a method call. Returns dynamic if the parent is
;; dynamic to accept any method call.
(define (get-type-of-method-call name args)
  (let* ([name (insert-implicit-self (unwrap name))]
         [parent-type (grace:member-parent (unwrap name))])
    (if (check-if-dynamic parent-type)
        ; Return dynamic if parent is dynamic.
        parent-type

        ; Find the method in the parent otherwise.
        (let* ([method-rtype (expression-type name)]
               [parent-name parent-type]
               [method-id (unwrap (grace:member-name name))]
               [name-string (unwrap (grace:identifier-value method-id))]
               [method (find-method-in name-string parent-name)]
               [args (unwrap-list args)]
               [params (get-field signature method)])

          (if (not (equal? (length args) (length params)))
              ; If we have the wrong number of arguments sent, error.
              (tc-error "method ~a requires ~a arguments, but got ~a"
                        name-string
                        (length params)
                        (length args))

              ; Else, check each param and make sure the types match.
              (begin
                (map (λ (arg param)
                       ; Get the types of the parameter and argument.
;                       (let* ([param-type (or (get-type
;                                               (grace:identifier-value
;                                                (grace:identifier-type
;                                                 (unwrap param))))
;                                              (new grace:type:dynamic*%))]
                       ; TODO REMOVE
                       (let* ([param-type-defined (grace:identifier-type
                                                   (unwrap param))]
                              [param-type (if param-type-defined
                                              (get-type
                                               (grace:identifier-value
                                                param-type-defined))
                                              (new grace:type:dynamic*%))]
                              [arg-type (expression-type arg)])

                         ; If they don't match up, error.
                         (unless (conforms-to? arg-type param-type)
                           (tc-error
                            "argument in ~a must be of type ~a, given ~a"
                            name-string
                            (send param-type readable-name)
                            (send arg-type readable-name)))))

                     ; Map the lambda function on args and params.
                     args
                     params)

                ; If we get through the map without triggering an error, give
                ; the return type of the method.
                method-rtype))))))


;; Returns an object with name determined by the place of the object's
;; declaration in the syntax and with methods as given in the body.
(define (get-type-of-object body)
  (let* ([inner-methods (foldl
                         body-stmt-to-method-type
                         (list)
                         (unwrap-list body))])
    (new grace:type:object%
         [methods inner-methods]
         [internal-name (format "Object_~a" (syntax-position (stx)))])))


;; Takes the one statement in the body of an object and returns a method
;; as declared or to access and change internal variables of an object.
(define (body-stmt-to-method-type body-stmt method-type-list)
  (if (syntax? body-stmt)
      ; If the body is a syntax element, unwrap it.
      (parameterize ([stx body-stmt])
        (body-stmt-to-method-type (syntax-e body-stmt) method-type-list))

      ; Else look for def, var, and method declarations in the body.
      (match body-stmt

        ; A definition has a getter.
        ((grace:def-decl name type value)
         ; @@@@ TODO: The conversion to name-string may not be needed
         (let* ([name-string (grace:identifier-value (unwrap name))])
           (append
            method-type-list
            (list (new grace:type:method%
                       [name (string->symbol name-string)]
                       [signature (list)]
                       [rtype (resolve-identifier type)])))))

        ; A variable has a getter and setter.
        ((grace:var-decl name type value)
         (let* ([name-string (grace:identifier-value (unwrap name))])
           (append
            method-type-list
            (list
              (new grace:type:method%
                  [name (string->symbol name-string)]
                  [signature (list)]
                  [rtype (resolve-identifier type)])
              (new grace:type:method%
                  [name (string->symbol (format "~a:=" name-string))]
                  [signature (list (same-other (resolve-identifier type)))]
                  ;[signature (list)]
                  [rtype (get-type "Done")])))))

        ; A method declaration is added to the method list.
        ((grace:method method-name signature body rtype)
         (let* ([name-string (grace:identifier-value (unwrap method-name))])
           (append
            method-type-list
            (list (new grace:type:method%
                       [name (string->symbol name-string)]
                       [signature (unwrap signature)]
                       ;; TODO: This is wrong, need to check return type
                       ;; of last statement in method.
                       [rtype (resolve-identifier rtype)])))))

        (else method-type-list))))


;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @@@@@ DEFINED: HOOKS AND TOOLS FOR THE INFERENCER. @@@@@
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;; List of items to be inferenced.
(define inference-list (list))


;; Adds an entry to a global list of inferenced things. This list is used
;; by the inferencing tool.
;;
;; Entries are very specific and are lists with these items:
;;
;;  start
;;    - char count until the inferenced thing
;;  end
;;    - char count until where the type annotation should be inserted
;;  var-name
;;    - the inferenced thing's name (i.e. for var foo -> foo)
;;  annotation-string
;;    - string of annotation to insert
;;  typedef-string
;;    - string of custom type, if inferencing an object's type
;;  primitive?
;;    - a boolean indicating whether the thing is an object
;;  inf-type
;;    - informs whether inferencing a var or def, etc.
;;
;; Alas, these should probably be in a struct or something.
(define (inference-hook start
                        end
                        var-name
                        var-type
                        value-type
                        inf-type)

  ; Check if the value is a primtive.
  (define primitive? (not (is-a? value-type grace:type:object%)))

  ; If and only if the value is an object, get its readable name so that we can
  ; add it as a type declaration (or rather, an object type declaration).
  (define typedef-string
    (or (and primitive? 'prim)
        (format "~a\n" (send value-type readable-name))))

  ; For both primitives and objects, get the name to add as a type annotation.
  (define annotation-string
    (or (and primitive? (send value-type readable-name))
        (get-field internal-name value-type)))

  ; For objects, check if the type has already been declared in our env.
  (when (not primitive?)
    (define existing-type
      (findf
       (λ (x) (equal? value-type x))
       (hash-values (env))))

    ; Don't decalre the type again, and annotate using the existing type.
    (when existing-type
      (set! typedef-string "")
      (set! annotation-string (get-field internal-name existing-type))))

  ; If the type of the variable was not given, add it to our inference list.
  (when (equal? var-type (new grace:type:dynamic*%))
    (set! inference-list
          (append inference-list
                  (list (list start
                              end
                              var-name
                              annotation-string
                              typedef-string
                              primitive?
                              inf-type))))))


;; Filters the inference list to get only primitives.
(define (infer-prims syntax-root)
  (set! inference-list empty)
  (typecheck syntax-root)
  (filter
   (λ (x) (match-define
            (list start
                  end
                  var-name
                  type-name
                  type-def
                  primitive?
                  inf-type)
            x)
     primitive?)
   inference-list))


;; Filters the inferece list to get only objects.
(define (infer-objects syntax-root)
  (set! inference-list empty)
  (typecheck syntax-root)
  (filter
   (λ (x) (match-define
            (list start
                  end
                  var-name
                  type-name
                  type-def
                  primitive?
                  inf-type)
            x)
     (not primitive?))
   inference-list))


;; Runs the typechecker and returns the entire list of inferenced items.
(define (infer-types syntax-root)
  (set! inference-list empty)
  (typecheck syntax-root)
  inference-list )


;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @@@@@ PROVIDES: typecheck, infer-types, infer-prims, infer-objects @@@@@
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(provide
 typecheck
 infer-types
 infer-prims
 infer-objects)


; @@@@@ DEBUGGING CODE @@@@@
; @@@@@ FIXME: REMOVE  @@@@@
(define (p in)
  (parse (object-name in) in))

(define a (p (open-input-string "
method foo() {
  return 1
}

def a = object {
//  def b = foo()
} 
")))

(display
  (typecheck a))
