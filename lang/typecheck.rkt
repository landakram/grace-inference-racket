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
     (values (grace:type:number) e))
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
              (set-type e name value-type) 
              (values value-type e))
            (error `typecheck 
                   "type annotation ~a on ~a does not match assignment type ~a" 
                   (grace:identifier-value type)
                   (grace:identifier-value name)
                   value-type))
        
      )
    )
  ))

(define (sanitize code-seq)
  (match code-seq
    ((grace:code-seq code)
     (grace:code-seq (filter (lambda (x) (not (list? x))) code)))
    (_ (code-seq))))

(define (typecheck thing)
  (let ((env (make-hash)))
    (set-type env "Number" (grace:type:number))
    (set-type env "String" (grace:type:string))
    (set-type env "List" (grace:type:boolean))
    (set-type env "Boolean" (grace:type:boolean))
    (set-type env "Dynamic" (grace:type:boolean))
    (set-type env "Void" (grace:type:boolean))
    (set-type env "Done" (grace:type:done))
    
    (let-values ([(t e)(type-check (sanitize thing) env)])
      (display t)
      (display e)
      e)
    
    ))
      

(provide typecheck)
  
    