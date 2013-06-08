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
            ((grace:method name signature body type)
             ;(test-eval body env)
             ;(print `(,(test-eval body env)))
             (env-lookup (env-extend* env `(,(test-eval name env)) `(,body)) 'foo)
             )
            ((grace:number num)
             (print (syntax-e num)))
            ((grace:var-decl iden bool val)
             (eval-initvar (test-eval iden env) val env))
            ((grace:identifier id bool)
             (string->symbol (syntax->datum id)))
            ((grace:method-call method params)
             (match (get-method (syntax->datum method) env) 
               [`(primitive ,p)
                ; =>
                (apply p (test-eval params env))]
               (else (print (get-method (syntax->datum method))))))
            ((grace:str str)
             (syntax-e str))
            (else (print "inelse") (print elt)))))) ; I don't think things will go here right now

(define (get-method elt env)
  (match elt
    ((grace:identifier name bool)
     (env-lookup env (string->symbol name)))))


;This is the preferred way to initialize var objects
;Makes a hidden var with the name $____, which should never be accessed by user 
;(change encoding to use a char that can't be entered in Grace or to make hiddenvar inaccessible except by internal methods?)
;Makes a method with same names as var that returns value of hiddenva
;Makes a method with name ____:= that takes a value and sets hiddenvar to that value
(define (eval-initvar var val env)
  (let* ((hiddenvar (string->symbol (string-append "$" (symbol->string var))))
         (readmethod var)
         (modmethod (string->symbol (string-append (symbol->string var) ":=")))
         (env0 (env-extend* env (list hiddenvar) (list val)))
         (env1 (env-extend* env0 (list readmethod) (list (test-eval `(lambda () ,hiddenvar) env0))))
         (env2 (env-extend* env1 (list modmethod) (list (test-eval `(lambda (x) (set! ,hiddenvar x)) env1)))))
    env2))

;Allows you to initialize a list of var objects.  Takes list of vars and list of initial values.
(define (eval-initvar* objs vals env)
  (match `(,objs ,vals)
    [`((,o . ,objs) (,v . ,vals))
     ;=>
     (eval-initvar* objs vals (eval-initvar o v env))]
    
    
    [`(() ())
     ; =>
     ;(display env)
     ;(newline)
     env]))


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

(define (AST-to-RG elt)
  (if (syntax? elt)
      (parameterize ((stx elt))
        (AST-to-RG (syntax-e elt)))
      (if (list? elt)
          (begin
            (if (eq? 1 1)
                (map AST-to-RG elt)
                (print (length elt))))
          (match elt
            ((grace:code-seq num) (AST-to-RG (cdr (syntax->datum num))))
            ((grace:object body) (print "body") (print  body) (string-append "(objectC () ()" (foldr string-append " "(AST-to-RG body)) ")"))
            ;(cadr )
            ((grace:method name signature body type) 
             (string-append 
              "(" (AST-to-RG name) "(lambda (" 
              (foldr string-append ") " (map (lambda (x) (string-append " " x)) (AST-to-RG signature)))
              (car (AST-to-RG body))))
            ((grace:method-call name args) (string-append "((send2 self " (AST-to-RG name) ") " (AST-to-RG (car args)) ")"))
            ;((grace:method-call name args) (print (car (AST-to-RG args)))) 
            ((grace:identifier value type) value)
            ((grace:str str) (string-append "\"" str "\""))
            (else (print elt))))))
(define (p in) (parse (object-name in) in))
(define a (p (open-input-string "
object{ method foo(x, y) {
print(y)
}
}
")))
(print (AST-to-RG (syntax-e a)))
;(display (syntax->datum a))
;(map (lambda (x) (test-eval x (env-initial))) (syntax->list (grace:code-seq-code (syntax-e a))))