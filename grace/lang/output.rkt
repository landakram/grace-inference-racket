#lang racket

(require "ast.rkt"
         "parse.rkt")

(define stx (make-parameter #f))

(define (test-eval elt env)
  (if (syntax? elt)
      (parameterize ((stx elt))
        (test-eval (syntax-e elt) env))
      (if (list? elt)
          (map (lambda (x) (test-eval x env)) elt)        
          (match elt
            ((grace:code-seq num) ; I don't think things will go here right now
             (begin (print "in codeseq")(test-eval (cdr (unwrap num)) env)))
            ((grace:number num)
             (print (syntax-e num)))
            ((grace:method-call method params)
             (match (get-method (syntax->datum method) env) 
               [`(primitive ,p)
                ; =>
                (apply p (test-eval params env))]))
            ((grace:str str)
             (syntax-e str))
            (else (print "inelse")))))) ; I don't think things will go here right now

(define (get-method elt env)
  (match elt
    ((grace:identifier name bool)
     (env-lookup env (string->symbol name)))))




;; Environments map variables to mutable cells 
;; containing values.  Also have list of objects.

(define-struct cell ([value #:mutable]))

; empty environment:
(define (env-empty) (hash))

; initial environment, with bindings for primitives:
(define (env-initial)
  (env-extend* 
   (env-extend* 
    (box (env-empty))
    ;first takes a whole list of primitives and binds them to racket equivalents
    ;many of these will need to be replaced: 
    ;all the math ones will need to extract values out of new number objects and then call primitive version rather than being in current form
    '(+  -  /  *  %   <= >= eq? void  print  newline string-append cons list eval false? number->string null list? == 
         prnt)
    (map (lambda (s) (list 'primitive s))
         `(,+ ,- ,/ ,* ,modulo ,<= ,>= ,eq? ,void ,print ,newline ,string-append ,cons ,list ,eval ,false? ,number->string ,null ,list? ,equal? 
              ,(lambda (x) (match x 
                             ;if x is an object, check if it has an asString method defined, and call that
                             ;ideally, all objects will have asString defined
                             [(? box?) (if (hash-has-key? (unbox x) 'asString) (begin (display (eval `asString x)) (newline)) (begin (display x) (newline)))]
                             [any (begin (display x) (newline))])))))
   ;next, extends environment further to add tru and fals objects
   ;naming is intentional to avoid any confusion with primitive true and false but should be changeable without causing any issues.
   ;The only thing users should see is the asString method, so it may be fine as-is.
   '(tru fals 
         ;<
         )
   (let* ((env0 (env-extend* (box (env-empty)) '(asString) `("true"))) 
          (env1 (env-extend* (box (env-empty)) '(asString) `("false")))) 
     (begin (set-box! env0 (unbox (env-extend* env0 '(not prefix!) `((closure (lambda () ,env1) ,env1) (closure (lambda () ,env1) ,env1)))))
            (set-box! env1 (unbox (env-extend* env1 '(not prefix!) `((closure (lambda () ,env0) ,env0) (closure (lambda () ,env0) ,env0)))))
            `(,env0 ,env1))))
  )


; looks up a value:
(define (env-lookup env var)
  (match (hash-ref (unbox env) var)
    [(? cell?)  
     (cell-value (hash-ref (unbox env) var))]
    [x
     (hash-ref (unbox env) var)]))


; sets a value in an environment:
(define (env-set! env var value)
  (set-cell-value! (hash-ref (unbox env) var) value))


; extends an environment with several bindings:
(define (env-extend* env varbls values)
  (match `(,varbls ,values)
    [`((,v . ,varbls) (,val . ,values))
     ; =>
     (env-extend* (box (hash-set (unbox env) v (make-cell val))) varbls values)]
    
    [`(() ())
     ; =>
     env]))

;extends environment, but instead of mapping var to a mutable cell containing the value, just maps it directly to the value
;eventually, set! method will change, and then we should change this so it looks like initvar, but without the var:= method.
(define (env-extdef* env invarbls values)
  (match `(,invarbls ,values)
    [`((,i . ,invarbls) (,val . ,values))
     ; =>
     (set-box! env (box (env-extend* (hash-set (unbox env) i val) invarbls values)))]
    
    [`(() ())
     ; =>
     env]))

;extend an environment mapping a list of values to void.
;Doesn't entirely match grace behavior: racket will allow you to call something like print on void
;while grace will call an error if you try to call a method on a var that is not yet mapped to anything.
(define (env-extempty env varbls)
  (match `(,varbls)
    [`((,v . ,varbls))
     ; =>
     (set-box! env (box (env-extempty (hash-set (unbox env) v (make-cell (void))) varbls)))]
    
    [`(())
     ; =>
     env]))


; mutates an environment with several assignments:
(define (env-set!* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     ; =>
     (begin
       (env-set! env v val)
       (env-set!* env vars values))]
    
    [`(() ())
     ; =>
     (void)]))

(define (p in) (parse (object-name in) in))
(define a (p (open-input-string "
print (\"Hello, World!\")
")))

(map (lambda (x) (test-eval x (env-initial))) (syntax->list (grace:code-seq-code (syntax-e a)))) 