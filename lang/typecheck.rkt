#lang racket

(require "ast.rkt"
         "parse.rkt")

;; Maps identifier -> type.
(define identifier-env (make-hash))

;; Gets a type from identifier-env. If the identifier is 
;; not present, returns false.
(define (get-type k) 
  (hash-ref (env) k #f))

;; Sets a type for a given identifier
(define (set-type k v) (hash-set! (env) k v))

(define stx (make-parameter #f))
(define env (make-parameter (make-hash)))
(define selftype (make-parameter (new grace:type:module%)))
(define is-object? (make-parameter #f))
(define current-return-type (make-parameter #f))

(define (tc-error msg . rest) 
  (raise-syntax-error 'typecheck (apply format msg rest) (stx)))

(define (add-method-to-selftype method) 
  (set-field! methods (selftype) (cons method (get-field methods (selftype)))))

(define (conforms-to? conforming-type type)
  (cond 
    ((equal? type (new grace:type%)) #t)
    ((equal? conforming-type type) #t)
    (else #f)))

(define (resolve-identifiers-list lst)
  (parameterize ((env (hash-copy (env))))
    (map maybe-bind-name lst)
    (map resolve-identifiers lst)
    (map expression-type lst)))

(define (find-method-in name parent)
  (findf 
   (lambda (a) (equal? (get-field name a) name))
   (get-field methods (resolve-identifier parent))))  
  
(define (find-method name)
;  (display name)
  (if (syntax? name)
      (parameterize ((stx name))
        (find-method (syntax->datum name)))
      ;; first search in selftype
      (let* ([method (findf 
                      (lambda (a) (equal? (get-field name a) (grace:identifier-value name)))
                      (get-field methods (selftype)))])
        (if (method) 
            method
            (get-type (grace:identifier-value name))))))

(define (insert-implicit-self method-name)
  (if (grace:member? method-name)
      method-name
      (grace:member (grace:identifier 
                     (datum->syntax (stx) "self" (stx))
                     #f) 
                    method-name)))

(define (unwrap possible-stx-obj)
  (if (syntax? possible-stx-obj)
      (syntax->datum possible-stx-obj)
      possible-stx-obj))

(define (unwrap-list possible-stx-obj)
  (if (syntax? possible-stx-obj)
      (syntax->list possible-stx-obj)
      possible-stx-obj))

;; Returns the actual type of a grade expression, or Dynamic for everything else
;; grace-struct -> (grace:type:....%) or (grace:type%)
(define (expression-type elt)
  (if (syntax? elt)
      (parameterize ((stx elt))
       ; (display elt)
       ; (display "\n\n")
;        (display (format "expression-type: ~a \n" elt))
        (expression-type (syntax-e elt)))
      
      (match elt
        ((grace:number value)
         ;(display "number\n")
         (new grace:type:number%))
        ((grace:str value)
         (new grace:type:string%))
        ((grace:identifier value type-identifier)
       ;  (display "++++\n")
       ;  (display value)
       ;  (display "\n++++\n")
         (resolve-identifier (grace:identifier (unwrap value) type-identifier)))
        ((grace:expression op e1 e2)
         (let* ((e1-type (expression-type e1))
                (e2-type (expression-type e2))
                (op (unwrap op))
                (member-op (findf 
                            (lambda (a) (equal? (get-field name a) op))
                            (get-field methods e1-type))))
           (if member-op
               (let ((param-type
                      (get-type (grace:identifier-value 
                                 (grace:identifier-type 
                                  (car (get-field signature member-op)))))))
             ;    (display param-type)
             ;    (display "\n")
             ;    (display e2-type)
             ;    (display e2)
             ;    (display "\n")
        ;         (display e2)
                 (if (conforms-to? e2-type param-type)
                     (get-type (grace:identifier-value (get-field rtype member-op)))
                     
                     (tc-error
                      "~a takes ~a but got ~a"
                      op
                      (send param-type readable-name)
                      (send e2-type readable-name))))
               (tc-error
                "no such operator ~a in ~a"
                op (send e1-type readable-name)))))
        ((grace:member parent name)
         (let* ((parent-type (expression-type parent))
                (name-string (unwrap (grace:identifier-value (unwrap name))))
                (member-op (findf 
                            (lambda (a) (equal? (get-field name a) name-string))
                            (get-field methods parent-type))))
           (if member-op
               (get-field rtype member-op )
              ; member-op
              ; (if (empty? (get-field signature member-op))
              ;     (get-type (grace:identifier-value (get-field rtype member-op)))
              ;     (tc-error
              ;     "method ~a in ~a requires ~a arguments, not 0"
              ;      (send member-op readable-name)
              ;      (send parent-type readable-name)
              ;      (length (get-field signature member-op))))
               (tc-error
                "no such method ~a in ~a"
                name-string
                (send parent-type readable-name)))))
        ((grace:method-call name args)
         (let* ((name (insert-implicit-self (unwrap name)))
                (method-rtype (expression-type name))
                (parent-type (expression-type (grace:member-parent name)))
                (name-string (unwrap (grace:identifier-value (unwrap (grace:member-name name)))))
                (member-op (findf 
                            (lambda (a) (equal? (get-field name a) name-string))
                            (get-field methods parent-type)))
                (args (unwrap-list args))
                (params (get-field signature member-op)))
           ;; Check that argument types match parameter types.
           (if (not (equal? (length args) (length params)))
               (tc-error "method ~a requires ~a arguments, not ~a"
                         name-string
                         (length params)
                         (length args))
               (begin (map (lambda (arg param)
                      (let* ((param-type (get-type (grace:identifier-value 
                                                    (grace:identifier-type 
                                                     param))))
                             (arg-type (expression-type arg)))
                        (if (conforms-to? param-type arg-type)
                            param-type
                            (tc-error
                             "argument in ~a must be of type ~a, given ~a"
                             name-string
                             (send param-type readable-name)
                             (send arg-type readable-name)))))
                    args
                    params)
               method-rtype))))
        ((grace:object body)
         (let* ((inner-methods (foldl body-stmt-to-method-type (list) (unwrap-list body))))
           (new grace:type:object% (methods inner-methods))))
        
        (else (new grace:type%)))))
  
(define (body-stmt-to-method-type body-stmt method-type-list)
  (if (syntax? body-stmt)
      (parameterize ((stx body-stmt))
        (body-stmt-to-method-type (syntax-e body-stmt) method-type-list))
      
      
      (match body-stmt 
        ((grace:def-decl name type value)
         (append method-type-list 
                 (list 
                  (new grace:type:method% 
                       [name (string->symbol name)]
                       [signature (list)]
                       [rtype (resolve-identifier type)]))))
        ((grace:var-decl name type value)
         (append method-type-list 
                 (list 
                  (new grace:type:method% 
                       [name (string->symbol (grace:identifier-value (unwrap name)))] 
                       [signature (list)] 
                       [rtype (resolve-identifier type)])
                  (new grace:type:method% 
                       [name (string->symbol (format "~a:=" name))] 
                       [signature (list (same-other (resolve-identifier type)))]
                       [rtype (resolve-identifier type)]))))
        ((grace:method method-name signature body rtype)
         (append method-type-list
                 (list
                  (new grace:type:method%
                       [name (string->symbol (grace:identifier-value (unwrap method-name)))]
                       [signature (unwrap signature)]
                       [rtype rtype]))))
        (else method-type-list))))

;; will use resolve-identifier for identifiers in expression-type

;; Returns the type of a given grace identifier in the current environment
;; (grace:identifier) -> (grace:type:...%)
(define (resolve-identifier ident)
;  (display "+-----\n")
  (display ident)
  (display "\n")
;  (display "\n------\n")
 ; (if (syntax? ident)
 ;     (parameterize ((stx ident))
 ;       (resolve-identifier (syntax->datum ident)))
  (if (false? ident)
      (new grace:type%)
      (get-type (grace:identifier-value (unwrap ident)))))

(define (resolve-identifiers elt)
  (if (syntax? elt)
      (parameterize ((stx elt))
        (resolve-identifiers (syntax-e elt)))
      
      (match elt
        ((grace:identifier value type-identifier)
         (resolve-identifier (grace:identifier value type-identifier)))
        ((grace:expression op e1 e2)
         (begin (resolve-identifiers e1)
                (resolve-identifiers e2)))
        ((grace:method-call name args)
         (begin (resolve-identifiers name)
                (resolve-identifiers-list (syntax->list args))))
        ((grace:member parent name)
         (begin (resolve-identifiers parent)))
        ;; TODO: array, matchcase, catchase
        
        ((grace:method method-name signature body rtype)
         (parameterize ((env (hash-copy (env))))
           (for ([param (syntax->list signature)])
             (resolve-identifier param)
             (set-type (grace:identifier-value (syntax->datum param)) 
                       (resolve-identifier (grace:identifier-type (syntax->datum param)))))
         ;  (resolve-identifiers-list (syntax->list signature))
           (parameterize ((current-return-type (resolve-identifier rtype)))
             (if (false? (current-return-type))
                 (tc-error "return type of method not defined as a type")
                 
                 (let* ([body-stmt-types (resolve-identifiers-list (syntax->list body))]
                        [last-statement (last (syntax->datum body))]
                        [real-type (expression-type last-statement)])
                   (if (not (grace:return? last-statement))
                       (if (not (conforms-to? real-type (current-return-type)))
                           (tc-error "returning type ~a from method of return type ~a"
                                     (send real-type readable-name)
                                     (send (current-return-type) readable-name))
                           ;; TODO: consideration for returning objects
                           real-type) ; do nothing
                       real-type)))))) ; do nothing
        ;; TODO: block
        ((grace:object body)
         (parameterize* ([selftype (new grace:type:object%)]
                         [env (hash-copy (env))]
                         [is-object? #t])
           (set-type "self" (selftype))
           (resolve-identifiers-list (syntax->list body))))
        
        ((grace:bind name value)
         (resolve-identifiers value)
         (let* ([name-type (expression-type name)]
                [value-type (expression-type value)])
           (cond
             ((grace:identifier? (unwrap name))
              (let* ([name-string (grace:identifier-value (unwrap name))])
                (cond
                  ;((is-a? name-type grace:type:def%) (tc-error "reassignment to constant ~a" name-string))
                  ((is-a? name-type grace:type:method%) (tc-error "assignment to method ~a" name-string))
                  ((false? name-type) (tc-error "assignment to undeclared ~a" name-string))
                  ((not (conforms-to? value-type name-type)) (tc-error "assigning value of nonconforming type ~a to var of type ~a"
                                                                       (send value-type readable-name)
                                                                       (send name-type readable-name)))
                  (else name-type))))
             ((grace:member? (unwrap name))
              (display (format "selftype methods: ~a\n" (map (lambda (x) (get-field name x)) (get-field methods (selftype)))))
              (let* ([member-op 
                      (find-method-in 
                       (format "~a:=" (grace:identifier-value (grace:member-name (unwrap name))) )
                       (grace:member-parent (unwrap name)))])
                (if member-op ;; ensures that we are working on a mutable var
                    (if (conforms-to? value-type name-type)
                        value-type
                        (tc-error "assigning value of nonconforming type ~a to var of type ~a"
                                  (send value-type readable-name)
                                  (send name-type readable-name)))
                    (tc-error "no such member"))))
             (else (grace:bind name value)))))
        ((grace:var-decl name type value) 
    ;     (display (env))
    ;     (display (format "------> var-decl ~a ~a ~a" name type value))
         (let* ([name-string (grace:identifier-value (unwrap name))]
                [name-type (resolve-identifier (unwrap name))]
                [_ (resolve-identifiers value)]
                [value-type (expression-type (unwrap value))]
                [type-type (resolve-identifier (unwrap type))])
           (if (not (conforms-to? value-type type-type))
               (tc-error "initializing var of type ~a with expression of type ~a"
                         (send type-type readable-name)
                         (send value-type readable-name))
               value-type)))
        ((grace:def-decl name type value)
         (let* ([name-string (grace:identifier-value (syntax->datum name))]
                [name-type (resolve-identifier name)]
                [_ (resolve-identifiers value)]
                [value-type (expression-type value)]
                [type-type (resolve-identifier type)])
           (if (not (conforms-to? value-type type-type))
               (tc-error "initializing def of type ~a with expression of type ~a"
                         (send type-type readable-name)
                         (send value-type readable-name))
               (value-type))))
        ((grace:return value)
 ;        (display value)
         (let* ([_ (resolve-identifiers value)]
                [value-type (expression-type value)])
           (cond 
             ((false? (current-return-type)) (tc-error
                                              "return statement with no surrounding method"))
             ((not (conforms-to? value-type (current-return-type))) (tc-error
                                                         "returning type ~a from method of return type ~a"
                                                         (send value-type readable-name)
                                                         (send (current-return-type) readable-name)))
             ;; types are equal
             (else value-type))))
        ;; TODO: index, op, if, while
        (else elt)
        )))

(define (maybe-bind-name elt)
  (display (format "is-object? ~a\n" (is-object?)))
  (if (syntax? elt)
      (parameterize ((stx elt))
        (maybe-bind-name (syntax-e elt)))
      (match elt
        ;; TODO: types
        ((grace:var-decl name type value)
         (let* ([name-string (grace:identifier-value (syntax->datum name))]
                [type-type (resolve-identifier type)])
           (if (is-object?)
               (begin (set-type name-string type-type) 
                      (add-method-to-selftype (new grace:type:method%
                                                   [name name-string]
                                                   [signature (list)]
                                                   [rtype type-type]))
                      (add-method-to-selftype (new grace:type:method%
                                                   [name (format "~a:=" name-string)]
                                                   [signature (list (same-other (resolve-identifier type)))]
                                                   [rtype type-type])))
               (set-type name-string type-type))))
        ((grace:def-decl name type value)
         (let* ([name-string (grace:identifier-value (syntax->datum name))]
                [type-type (resolve-identifier type)])
           (if (is-object?)
               (begin (set-type name-string type-type) 
                      (add-method-to-selftype (new grace:type:method%
                                                   [name name-string]
                                                   [signature (list type-type)]
                                                   [rtype type-type])))
               (set-type name-string type-type))))
        ((grace:method name signature body rtype)
         (let* ([type-type (resolve-identifier rtype)]
                [method-name (grace:identifier-value (syntax->datum name))]
                [new-method-type (new grace:type:method% 
                                     (name method-name)
                                     (signature (syntax->datum signature))
                                     (rtype type-type))])
           (add-method-to-selftype new-method-type)
           (set-type (grace:identifier-value (unwrap name)) 
                     type-type)))
        (else elt))))
      ;; TODO: inherits, class, import
      
        
         
         
         
        

;; Removes all empty statements if (grace:code-seq? code-seq)
(define (sanitize code-seq)
  (match code-seq
    ((grace:code-seq code)
     (grace:code-seq (filter (lambda (x) (not (list? x))) code)))
    (_ code-seq)))

(define (typecheck thing)
  (parameterize ([env (make-hash)]
                 [stx thing])
    (set-type "Number" (new grace:type:number%))
    (set-type "String" (new grace:type:string%))
    (set-type "List" (new grace:type:list%))
    (set-type "Boolean" (new grace:type:boolean%))
    (set-type "Dynamic" (new grace:type%))
    (set-type "Void" (new grace:type:void%))
    (set-type "Done" (new grace:type:done%))
    
    (set-type "Object" (new grace:type:object%))
    
    (set-type "true" (new grace:type:boolean%))
    (set-type "false" (new grace:type:boolean%))
    
    (set-type "self" (selftype))
    
    (resolve-identifiers-list 
     (syntax->list (grace:code-seq-code (syntax-e thing))))))
      
(provide typecheck)
 
(define (p in) (parse (object-name in) in))
(define a (p (open-input-string "\nvar a : Dynamic := object {\n var a : Number := 4 \n var b : Number := 7 \nmethod bar(s : String) -> Number {\n47 \n}\n}")))
(map (lambda (x) (send x readable-name)) (typecheck a))