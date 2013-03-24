#lang racket
(define-for-syntax (grace-struct-syntax prefix stx) 
  (syntax-case stx ()
    [(_ (struct-name (field ...) struct-option ...) ...)
     (with-syntax ([(grace:struct ...) (map (lambda (id)
                                              (datum->syntax
                                               id
                                               (string->symbol
                                                (format "~a:~a" prefix (syntax-e id)))))
                                            (syntax->list (syntax (struct-name ...))))])
       (syntax (begin (define-struct grace:struct (field ...) struct-option ... #:prefab) ...)))]))

(define-syntax (define-grace-structs stx)
  (grace-struct-syntax "grace" stx))

(define-syntax (define-grace-types stx)
  (grace-struct-syntax "grace:type" stx))

(define-grace-structs
  (number (value))
  (str (value))
  (identifier (value type))
  
  (var-decl (name type value))
  (def-decl (name type value))
  (bind (name value))
  
  (expression (op e1 e2))
  (method-call (name args))
  (object (body))
  (method (name signature body type))
  (member (parent name))
  (return (value))
  
  (code-seq (code)))

(define number-identifier (grace:identifier "Number" #f))
(define string-identifier (grace:identifier "String" #f))
(define boolean-identifier (grace:identifier "Boolean" #f))
(define dynamic-identifier (grace:identifier "Dynamic" #f))
(define list-identifier (grace:identifier "List" #f))

(define number-other (grace:identifier "other" number-identifier))
(define boolean-othter (grace:identifier "other" boolean-identifier))
(define string-other (grace:identifier "other" string-identifier))
(define top-other (grace:identifier "other" dynamic-identifier))
(define list-other (grace:identifier "other" list-identifier))

(define (same-other type-identifier) (grace:identifier "_" type-identifier))

(define grace:type<%>
  (interface () readable-name))

(define grace:type%
  (class* object% (grace:type<%> equal<%>)
    (super-new)
    (init-field (methods (list)))
    (define/public (readable-name) 
      "Dynamic")
    (define/public (equal-to? other recur)
      (and (recur methods (get-field methods other))
           (recur (readable-name) (send other readable-name))))
    (define/public (equal-hash-code-of hash-code)
      (hash-code (readable-name)))
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (readable-name)))))

(define grace:type:dynamic%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (define/override (readable-name)
      "Dynamic")))

(define grace:type:number%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (set-field! 
     methods this (list 
               (new grace:type:method% [name +] [signature (list number-other)] [rtype number-identifier])
               (new grace:type:method% [name -] [signature (list number-other)] [rtype number-identifier])
               (new grace:type:method% [name *] [signature (list number-other)] [rtype number-identifier])
               (new grace:type:method% [name /] [signature (list number-other)] [rtype number-identifier])
               (new grace:type:method% [name modulo] [signature (list number-other)] [rtype number-identifier])
               (new grace:type:method% [name exp] [signature (list number-other)] [rtype number-identifier])
               
               (new grace:type:method% [name equal?] [signature (list number-other)] [rtype top-other])
               (new grace:type:method% [name 'not] [signature (list number-other)] [rtype top-other])
               (new grace:type:method% [name <] [signature (list number-other)] [rtype boolean-identifier])
               (new grace:type:method% [name >] [signature (list number-other)] [rtype boolean-identifier])
               (new grace:type:method% [name <=] [signature (list number-other)] [rtype boolean-identifier])
               (new grace:type:method% [name >=] [signature (list number-other)] [rtype boolean-identifier])))
    (define/override 
      (readable-name) "Number")))

(define grace:type:object%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (init-field internal-name)
    (define/override (readable-name) 
      (define o (open-output-string))
      (displayln (format "type ~a = {" internal-name) o)
      (for ([method methods])
        (displayln (format "    ~a" (send method readable-signature)) o))
      (displayln "}" o)
      (get-output-string o))
    (define/override (equal-to? other recur)
      (recur methods (get-field methods other)))))

(define (unwrap possible-stx-obj)
  (if (syntax? possible-stx-obj)
      (syntax->datum possible-stx-obj)
      possible-stx-obj))

(define grace:type:method%
  (class* grace:type% ()
    (super-new)
    (init-field name signature rtype) 
    (define/override 
      (readable-name) "Method")
    (define/public (rtype-name)
      (cond ((equal? rtype 'missing) "Dynamic")
            ((grace:identifier? rtype) (grace:identifier-value rtype))
            ((is-a? rtype grace:type%) (send rtype readable-name))))
    (define/public (readable-signature)
      (define o (open-output-string))
      (display (format "~a(" name) o)
      (for ([t signature])
        (define unwrapped (unwrap t))
        (display t)
        (define type-name (cond ((equal? unwrapped 'missing) "Dynamic")
                               ((grace:identifier? unwrapped) (grace:identifier-value unwrapped))
                               ((is-a? unwrapped grace:type%) (send unwrapped readable-name))))
        (display (format "~a : ~a" 
                         (grace:identifier-value unwrapped)
                         type-name)))
      (display (format ") -> ~a" (rtype-name))  o)
      (get-output-string o))
    (define/override (equal-to? other recur)
      (displayln "******")
      (displayln name)
      (displayln (get-field name other))
      (displayln signature)
      (displayln (get-field signature other))
      (displayln "******")
      (and 
       (recur name (get-field name other))
       (recur (readable-signature) (send other readable-signature))
       (recur (rtype-name) (send other rtype-name))))))

(define grace:type:string%
  (class* grace:type% ()
    (super-new)
    (define/override (readable-name) "String")))

(define grace:type:list%
  (class* grace:type% ()
    (super-new)
    (define/override (readable-name) "List")))

(define grace:type:boolean%
  (class* grace:type% ()
    (super-new)
    (define/override (readable-name) "Boolean")))

(define grace:type:void%
  (class* grace:type% ()
    (super-new)
    (define/override (readable-name) "Void")))

(define grace:type:done%
  (class* grace:type% ()
    (super-new)
    (define/override (readable-name) "Done")))

(define grace:type:module%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (define/override (readable-name) "Module")))
  
(provide (all-defined-out))