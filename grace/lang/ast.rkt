#lang racket

(define-for-syntax (grace-struct-syntax prefix stx)
  (syntax-case stx ()
    [(_ (struct-name (field ...) struct-option ...) ...)
     (with-syntax
       ([(grace:struct ...)
         (map (λ (id)
                 (datum->syntax
                   id
                   (string->symbol (format "~a:~a" prefix (syntax-e id)))))
              (syntax->list (syntax (struct-name ...))))])
       (syntax
         (begin
           (define-struct grace:struct (field ...) struct-option ... #:prefab)
           ...)))]))

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
  (if-then (check body))
  (class-decl (name body))

  (code-seq (code)))

(define number-identifier (grace:identifier "Number" #f))
(define string-identifier (grace:identifier "String" #f))
(define boolean-identifier (grace:identifier "Boolean" #f))
(define dynamic-identifier (grace:identifier "Dynamic" #f))
(define list-identifier (grace:identifier "List" #f))
(define void-identifier (grace:identifier "Void" #f))
(define done-identifier (grace:identifier "Done" #f))
(define top-identifier (grace:identifier "Top" #f))

(define number-other (grace:identifier "other" number-identifier))
(define boolean-othter (grace:identifier "other" boolean-identifier))
(define string-other (grace:identifier "other" string-identifier))
(define top-other (grace:identifier "other" top-identifier))
(define list-other (grace:identifier "other" list-identifier))

;; Adds methods to a type where new-methods should be passed in as a (list ...)
(define (add-methods parent new-methods)
  (let* ([old-methods (get-field methods parent)]
         [all-methods (append new-methods old-methods)])
    (set-field! methods parent all-methods)))

(define (same-other type-identifier) (grace:identifier "_" type-identifier))

(define grace:type<%>
  (interface () readable-name))

(define grace:type:method%
  (class* object% (grace:type<%> equal<%>)
    (super-new)
    (init-field name signature rtype)
    (define/public (readable-name) "Method")
    (define/public (rtype-name)
      (cond ((equal? rtype 'missing) "Dynamic")
            ((grace:identifier? rtype) (grace:identifier-value rtype))
            ((is-a? rtype grace:type%) (send rtype readable-name))))
    (define/public (readable-signature)
      (define o (open-output-string))
      (display (format "~a(" name) o)
      ;(displayln signature) ; TODO REMOVE
      (for ([i (in-range (length signature))])
        (define t (list-ref signature i))
        (define unwrapped (grace:identifier-type (unwrap t)))
        ; TODO REMOVE
        ;(displayln t)
        ; (displayln unwrapped)
        (define type-name
          (cond ((equal? unwrapped #f) "Dynamic")
                ((grace:identifier? unwrapped)
                 (grace:identifier-value unwrapped))
                ((is-a? unwrapped grace:type%)
                 (send unwrapped readable-name))))
        (if (equal? i (- (length signature) 1))
            (display (format "_ : ~a" type-name) o)
            (display (format "_ : ~a, " type-name) o)))

      ;(for ([t signature])
      ;  (define unwrapped (grace:identifier-type (unwrap t)))
      ;  (displayln t)
      ;  (define type-name
      ;    (cond ((equal? unwrapped 'missing) "Dynamic")
      ;          ((grace:identifier? unwrapped)
      ;           (grace:identifier-value unwrapped))
      ;          ((is-a? unwrapped grace:type%)
      ;           (send unwrapped readable-name))))
      ;  (display (format "_ : ~a"
      ;                   type-name) o))

        ;(display (format "~a : ~a, "
        ;                 (grace:identifier-value unwrapped)
        ;                 type-name)))
      (display (format ") -> ~a" (rtype-name))  o)
      (get-output-string o))

    (define/public (equal-to? other recur)
      ;(displayln "******")
      ;(displayln name)
      ;(displayln (get-field name other))
      ;(displayln signature)
      ;(displayln (get-field signature other))
      ;(displayln "******")
      (and
       (recur name (get-field name other))
       (recur (readable-signature) (send other readable-signature))
       (recur (rtype-name) (send other rtype-name))))

    (define/public (equal-hash-code-of hash-code)
      (hash-code (readable-name)))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (readable-name)))))


(define builtin-methods
  (list
    (new grace:type:method%
         [name 'print]
         [signature (list top-other)]
         [rtype done-identifier])))

(define grace:type%
  (class* object% (grace:type<%> equal<%>)
    (super-new)
    (init-field (methods (list)))
    (init-field (builtins builtin-methods))
    (define/public (readable-name)
      "Dynamic")
    (define/public (equal-to? other recur)
      (and (recur methods (get-field methods other))
           (recur (readable-name) (send other readable-name))))
    (define/public (equal-hash-code-of hash-code)
      (hash-code (readable-name)))
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (readable-name)))))

(define (unwrap possible-stx-obj)
  (if (syntax? possible-stx-obj)
      (syntax->datum possible-stx-obj)
      possible-stx-obj))

(define grace:type:object%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (inherit-field builtins)
    (init-field internal-name)
    (define/override (readable-name)
      (define o (open-output-string))
      (displayln (format "type ~a = {" internal-name) o)
      (for ([method methods])
        ;(displayln method) ; TODO REMOVE
        ;(displayln (send method readable-signature))
        (displayln (format "    ~a" (send method readable-signature)) o))
      (displayln "}" o)
      (get-output-string o))
    (define/override (equal-to? other recur)
      (recur methods (get-field methods other)))))

(define grace:type:module%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (inherit-field builtins)
    (define/override (readable-name) "Module")))

; List of methods for number types.
(define number-methods
  (list
    (new grace:type:method%
         [name +]
         [signature (list number-other)]
         [rtype number-identifier])
    (new grace:type:method%
         [name -]
         [signature (list number-other)]
         [rtype number-identifier])
    (new grace:type:method%
         [name *]
         [signature (list number-other)]
         [rtype number-identifier])
    (new grace:type:method%
         [name /]
         [signature (list number-other)]
         [rtype number-identifier])
    (new grace:type:method%
         [name modulo]
         [signature (list number-other)]
         [rtype number-identifier])
    (new grace:type:method%
         [name exp]
         [signature (list number-other)]
         [rtype number-identifier])

    (new grace:type:method%
         [name equal?]
         [signature (list top-other)]
         [rtype boolean-identifier])
    (new grace:type:method%
         [name 'not]
         [signature (list number-other)]
         [rtype boolean-identifier])
    (new grace:type:method%
         [name <]
         [signature (list number-other)]
         [rtype boolean-identifier])
    (new grace:type:method%
         [name >]
         [signature (list number-other)]
         [rtype boolean-identifier])
    (new grace:type:method%
         [name <=]
         [signature (list number-other)]
         [rtype boolean-identifier])
    (new grace:type:method%
         [name >=]
         [signature (list number-other)]
         [rtype boolean-identifier])))

(define grace:type:number%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (add-methods this number-methods)
    (define/override
      (readable-name) "Number")))

; List of methods for string types.
; Empty for now. TODO: Implement if there are any.
(define string-methods
  (list
    (new grace:type:method%
         [name 'concat]
         [signature (list string-other)]
         [rtype string-identifier])))

(define grace:type:string%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (add-methods this string-methods)
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

(define grace:type:dynamic%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (define/override (readable-name)
      "Dynamic")
    (define/public (internal-name)
      "Dynamic")
    (define/override (equal-to? other recur)
      (if (recur (send other readable-name) "Dynamic")
          (recur (send other internal-name) (internal-name))
          #f))))

(define grace:type:dynamic*%
  (class* grace:type:dynamic% ()
    (super-new)
    (inherit-field methods)
    (define/override (internal-name)
      "Missing")))


(define grace:type:top%
  (class* grace:type% ()
    (super-new)
    (inherit-field methods)
    (define/override (readable-name)
      "Top")))

(provide (all-defined-out))
