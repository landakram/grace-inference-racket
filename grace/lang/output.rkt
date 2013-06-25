#lang racket

(require "ast.rkt"
         "parse.rkt")
(provide AST-to-RG)

(define stx (make-parameter #f))

;; Environments map variables to mutable cells 
;; containing values.  Also have list of objects.
(define-struct cell ([value #:mutable]))

; empty environment:
(define (env-empty) (hash))

; looks up a value:
(define (env-lookup env var)
  (match (hash-ref (unbox env) var)
    [(? cell?)  
     (cell-value (hash-ref (unbox env) var))]
    [x
     (hash-ref (unbox env) var)]))

; extends an environment with several bindings:
(define (env-extend* env varbls values)
  (match `(,varbls ,values)
    [`((,v . ,varbls) (,val . ,values))
     ; =>
     (env-extend* (box (hash-set (unbox env) v (make-cell val))) varbls values)]
    
    [`(() ())
     ; =>
     env]))

(define env-reverse 
  (env-extend* 
   (box (env-empty))
   ;first takes a whole list of primitives and binds them to racket equivalents
   ;(many of these will need to be replaced:
   ;all the math ones will need to extract values out of new number objects
   ;and then call primitive version rather than being in current form)
   `(,+ ,- ,/ ,* ,modulo ,<= ,>= ,eq? concat ,exp or and)
   (map (lambda (s) (list 'primitive s))
        '(+ -  /  *  %   <= >= eq? ++ expt or and))))

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
            ((grace:code-seq code) 
             (set! code (syntax->datum code))
             (string-append 
              "(objectC () (" (string-append* (extract-methods code))")" 
              "(begin (list" 
              (foldr string-append "" (all-but-methods code)) 
              ")))"))
            ;(if (list? num) '("") 
            ;(string-append* (map AST-to-RG (syntax->datum num)))))
            ((grace:object body)
             (string-append "(objectC () (" 
                            (string-append* (extract-methods body)) ")" 
                            "(begin (list" 
                            (foldr string-append "" (all-but-methods body))
                            ")))"))
            ((grace:method name signature body type)
             (string-append 
              "(" (dont-wrap name) "(lambda (" 
              (foldr string-append
                     ") " (map (lambda (x) (string-append " " x)) 
                               (dont-wrap signature)))
              "(begin (list "
              (string-append* (AST-to-RG body)) "))))"))
            ((grace:method-call name args) 
             (string-append "(" (dont-wrap name) " " 
                            (string-append* (AST-to-RG args)) ")"))
            ((grace:identifier value type) (string-append "(" value ")"))
            ((grace:var-decl name type value) 
             (string-append "(initvar " (dont-wrap name) " " (AST-to-RG value) ")"))
            ((grace:def-decl name type value)
             (string-append "(initdef " (dont-wrap name) " " (AST-to-RG value) ")"))
            ((grace:str str) (string-append "\"" str "\""))
            ((grace:number num) (number->string num))
            ((grace:expression op e1 e2)
             (string-append "(" (symbol->string (cadr (env-lookup env-reverse op)))
                            " " (AST-to-RG e1) " " (AST-to-RG e2) ")"))
            ((grace:member parent name) 
             (string-append "(send2 " (AST-to-RG parent) " " (AST-to-RG name) ")"))
            ((grace:bind name value)
             (match name
               ((grace:member parent child) 
                (string-append "((send2 " (AST-to-RG parent) " " (dont-wrap child)
                               ":=) " (AST-to-RG value) ")"))
                (else 
             (string-append "(" (dont-wrap name) ":= " (AST-to-RG value) ")"))))
            ((grace:if-then-else cond tbody ebody) 
             (string-append "(myif " (AST-to-RG cond)  
                            "(begin (list" (string-append* (AST-to-RG tbody)) 
                            ")) (begin (list" (string-append* (AST-to-RG ebody))
                            ")))"))
            (void "")
            (else (print elt))))))


(define (dont-wrap elt)
  (if (syntax? elt)
      (parameterize ((stx elt))
        (dont-wrap (syntax-e elt)))
      (if (list? elt)
          (begin
            (if (eq? 1 1)
                (map dont-wrap elt)
                (print (length elt))))
          (match elt
            ((grace:identifier value type) value)
            ((grace:member parent name) 
             (string-append "(send2 " (AST-to-RG parent) " " (dont-wrap name) ")"))
            (else (print elt))))))

(define (extract-methods elt)
  (if (syntax? elt)
      (parameterize ((stx elt))
        (extract-methods (syntax-e elt)))
      (if (list? elt)
          (begin
            (if (eq? 1 1)
                (map extract-methods elt)
                (print (length elt))))
          (match elt
            ((grace:method name signature body type) (AST-to-RG elt))
            (else "")))))

(define (all-but-methods elt)
  (if (syntax? elt)
      (parameterize ((stx elt))
        (all-but-methods(syntax-e elt)))
      (if (list? elt)
          (begin
            (if (eq? 1 1)
                (map all-but-methods elt)
                (print elt)))
          (match elt
            ((grace:method name signature body type) "  ")
            (else (AST-to-RG elt))))))

(define (p in) (parse (object-name in) in))

(define a (p (open-input-string "
var x := object {
    var y := object {
    var val := 1    
}
}
print(x.y.val)
x.y.val := 2
print(x.y.val)
")))
;(displayln (grace:object a))
;(displayln (syntax->datum a))
;(display (AST-to-RG (syntax-e a)))
