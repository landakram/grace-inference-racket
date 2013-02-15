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

;; grace-struct -> (type, new environment)
(define (type-check thing e)
  (match thing
    ((grace:code-seq (list-rest code code-seq))
     (let-values (((t new-e) (type-check code e)))
        (cond
          ((empty? code-seq) (values t new-e))
          (else (type-check (grace:code-seq code-seq) new-e)))))
    ((grace:number value)
     (values grace:type:number e))
    ((grace:str value)
     (values (grace:type:string) e))
    ((grace:identifier value type)
     (let ([ty (get-type e value)])
        (cond
          ((false? ty) (error `typecheck "~a referenced before assignment." value))
          (else (values ty e)))))
    ;; TODO: handle decl with no binding
    ((grace:var-decl name type value) 
     (let-values ([(value-type e1) (type-check value e)]
                   [(type-type e2) (type-check type e)])
        (if (equal? value-type type-type)
            (begin 
              (set-type e (grace:identifier-value name) value-type) 
              (values value-type e))
            (error `typecheck 
                   "type annotation ~a on variable ~a does not match assignment type ~a" 
                   (grace:identifier-value type)
                   (grace:identifier-value name)
                   value-type))))
    ((grace:def-decl name type value)
     (let-values ([(value-type e1) (type-check value e)]
                   [(type-type e2) (type-check type e)])
        (if (equal? value-type type-type)
            (begin 
              (set-type e name value-type) 
              (values value-type e))
            (error `typecheck 
                   "type annotation ~a on definition ~a does not match assignment type ~a" 
                   (grace:identifier-value type)
                   (grace:identifier-value name)
                   value-type))))
    ((grace:bind name value)
     (let-values ([(value-type e1) (type-check value e)]
                  [(name-type _) (type-check name e)])
       (if (equal? value-type name-type)
           (values value-type e)
           (error `typecheck 
                   "type ~a of variable ~a does not match assignment type ~a" 
                   name-type
                   (grace:identifier-value name)
                   value-type))))
    ((grace:expression op e1 e2)
     (let-values ([(e1-type env1) (type-check e1 e)]
                  [(e2-type env2) (type-check e2 e)])
       (let ((member-op (findf 
                        (lambda (a) (equal? (grace:type:method-name a) op))
                        (grace:type-methods e1-type))))
         (if member-op
             (let ((param-type
                   (get-type e 
                             (grace:identifier-value 
                              (grace:identifier-type 
                               (car (grace:type:method-signature member-op)))))))
               (if (equal? param-type e2-type)
                   (values (get-type e 
                                     (grace:identifier-value 
                                      (grace:type:method-rtype member-op))) 
                           e)
                   (error `typecheck
                          "passed argument of type ~a to parameter of type ~a"
                          e2-type 
                          param-type)))
             (error `typecheck
                    "no such operator ~a in ~a"
                    op e1-type)))))
     
     
                 
                 
  ))

;; Removes all empty statements if (grace:code-seq? code-seq)
(define (sanitize code-seq)
  (match code-seq
    ((grace:code-seq code)
     (grace:code-seq (filter (lambda (x) (not (list? x))) code)))
    (_ (code-seq))))

(define (typecheck thing)
  (let ((env (make-hash)))
    (set-type env "Number" grace:type:number)
    (set-type env "String" (grace:type:string))
    (set-type env "List" (grace:type:list))
    (set-type env "Boolean" (grace:type:boolean))
    (set-type env "Dynamic" (grace:type:dynamic))
    (set-type env "Void" (grace:type:void))
    (set-type env "Done" (grace:type:done))
    
    (set-type env "true" (grace:type:boolean))
    (set-type env "false" (grace:type:boolean))
    
    (let-values ([(t e)(type-check (sanitize thing) env)])
      e)))
      
(provide typecheck)
  
    