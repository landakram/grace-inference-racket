#lang racket

(require "ast.rkt"
         "parse.rkt")

;; Maps identifier -> type.
(define identifier-env (make-hash))

;; Gets a type from identifier-env. If the identifier is 
;; not present, returns false.
(define (get-type e k) 
  (hash-ref e k #f))

;; Sets a type for a given identifier
(define (set-type e k v) (hash-set! e k v))

(define stx (make-parameter #f))

(define (tc-error msg . rest) 
  (raise-syntax-error 'typecheck (apply format msg rest) (stx)))

(define (p in) (parse (object-name in) in))

(define a (p (open-input-string "\nvar a : Number := 4\nvar b : Number := 6")))

;; grace-struct -> (type, new environment)
(define (type-check thing e)
  (if (syntax? thing)
      (parameterize ((stx thing))
        (type-check (syntax-e thing) e))
      (match thing
        ((list) (values #f e))
        ((list-rest x xs) 
         (let-values (((t new-e) (type-check x e)))
           (cond
             ((empty? xs) (values t new-e))
             (else (type-check xs new-e)))))
        ((grace:code-seq c)
         (let* ((unwrapped (syntax->list c)))
           (let-values (((t new-e) (type-check unwrapped e)))
             (values t new-e))))
        ((grace:number value)
         (values (new grace:type:number%) e))
        ((grace:str value)
         (values (new grace:type:string%) e))
        ((grace:identifier value type)
         (let ([ty (get-type e (syntax->datum value))])
           (cond
             ((false? ty) (tc-error "~a referenced before assignment." value))
             (else (values ty e)))))
        ;; TODO: handle decl with no binding
        ((grace:var-decl name type value) 
         (let-values ([(value-type e1) (type-check value e)]
                      [(type-type e2) (type-check type e)])
           (if (equal? value-type type-type)
               (begin 
                 (set-type e (grace:identifier-value (syntax->datum name)) value-type) 
                 (values value-type e))
               (tc-error 
                      "type annotation ~a on variable ~a does not match assignment type ~a" 
                      (grace:identifier-value type)
                      (grace:identifier-value name)
                      (send value-type readable-name)))))
        ((grace:def-decl name type value)
         (let-values ([(value-type e1) (type-check value e)]
                      [(type-type e2) (type-check type e)])
           (if (equal? value-type type-type)
               (begin 
                 (set-type e name value-type) 
                 (values value-type e))
               (tc-error 
                      "type annotation ~a on definition ~a does not match assignment type ~a" 
                      (grace:identifier-value type)
                      (grace:identifier-value name)
                      (send value-type readable-name)))))
        ((grace:bind name value)
         (let-values ([(value-type e1) (type-check value e)]
                      [(name-type _) (type-check name e)])
           (if (equal? value-type name-type)
               (values value-type e)
               (tc-error 
                      "type ~a of variable ~a does not match assignment type ~a" 
                      (send name-type readable-name)
                      (grace:identifier-value name)
                      (send value-type readable-name)))))
        ((grace:expression op e1 e2)
         (let-values ([(e1-type env1) (type-check e1 e)]
                      [(e2-type env2) (type-check e2 e)])
           (let* ((op (syntax-e op))
                  (member-op (findf 
                             (lambda (a) (equal? (get-field name a) op))
                             (send e1-type methods))))
             (if member-op
                 (let ((param-type
                        (get-type e 
                                  (grace:identifier-value 
                                   (grace:identifier-type 
                                    (car (get-field signature member-op)))))))
                   (if (equal? param-type e2-type)
                       (values (get-type e 
                                         (grace:identifier-value 
                                          (get-field rtype member-op))) 
                               e)
                       (tc-error
                              "~a takes ~a but got ~a"
                              op
                              (send param-type readable-name)
                              (send e2-type readable-name))))
                 (tc-error
                        "no such operator ~a in ~a"
                        op (send e1-type readable-name))))))
        
        
        
        
  )))

;; Removes all empty statements if (grace:code-seq? code-seq)
(define (sanitize code-seq)
  (match code-seq
    ((grace:code-seq code)
     (grace:code-seq (filter (lambda (x) (not (list? x))) code)))
    (_ code-seq)))

(define (typecheck thing)
  (let ((env (make-hash)))
    (set-type env "Number" (new grace:type:number%))
    (set-type env "String" (new grace:type:string%))
    (set-type env "List" (new grace:type:list%))
    (set-type env "Boolean" (new grace:type:boolean%))
    (set-type env "Dynamic" (new grace:type%))
    (set-type env "Void" (new grace:type:void%))
    (set-type env "Done" (new grace:type:done%))
    
    (set-type env "true" (new grace:type:boolean%))
    (set-type env "false" (new grace:type:boolean%))
    
    (let-values ([(t e) (type-check (sanitize thing) env)])
      e)))
      
(provide typecheck)
  
    