#lang typed/racket

;;; TODO: Account for empty list '() that is inserted in the parser, either by
;;;   adding them to the acceptable types below or changing the use of empty.

(struct: method-type
  ([name : String]
   [signature : (Listof String)]
   [rtype : String])
  #:prefab)

(define-type type-name
  (U (Syntaxof grace:identifier) #f))

(struct: grace:number
  ([value : (Syntaxof Number)])
  #:prefab)

(struct: grace:str
  ([value : (Syntaxof String)])
  #:prefab)

(struct: grace:identifier
  ([value : String]
   [type  : type-name])
  #:prefab)

(struct: grace:method-def
  ([name      : (Syntaxof grace:identifier)]
   [signature : (Syntaxof (Listof (Syntaxof grace:identifier)))]
   ;[signature : (Listof (Syntaxof grace:identifier))]
   [rtype     : (Syntaxof grace:identifier)])
  #:prefab)

(struct: grace:type-def
  ([name    : (Syntaxof grace:identifier)]
   [methods : (Syntaxof (Listof (Syntaxof grace:method-def)))])
   ;[methods : (Listof (Syntaxof grace:method-def))])
  #:prefab)

(struct: grace:var-decl
  ([name  : (Syntaxof grace:identifier)]
   [type  : type-name]
   [value : Any])
  #:prefab)

(struct: grace:def-decl
  ([name  : (Syntaxof grace:identifier)]
   [type  : type-name]
   ;; TODO: possibly fix type of value
   [value : Any])
  #:prefab)

(struct: grace:bind
  ([name  : (Syntaxof grace:identifier)]
   ;; TODO: possibly fix type of value
   [value : Any])
  #:prefab)

(struct: grace:expression
  ([op  : Symbol]
   ;; TODO: Fix type of operands
   [lhs : Any]
   [rhs : Any])
  #:prefab)

(struct: grace:method-call
  ([name : (U (Syntaxof grace:identifier) (Syntaxof grace:member))]
   ;; TODO: Fix type of args
   [args : (Listof Any)])
  #:prefab)

(struct: grace:object
  ([body : (Listof (Syntaxof Any))])
  #:prefab)

(struct: grace:method
  ([name      : (Syntaxof grace:identifier)]
   [signature : (Listof (Syntaxof grace:identifier))]
   ;; TODO: Fix type of body
   [body : (Listof (Syntaxof Any))]
   [type : type-name])
  #:prefab)

(struct: grace:member
  ([parent : (U (Syntaxof grace:identifier) (Syntaxof grace:member))]
   [name   : (Syntaxof grace:identifier)])
  #:prefab)

(struct: grace:return
  ;; TODO: Fix type of value, maybe grace:expression or identifier.
  ([value : Any])
  #:prefab)

(struct: grace:if-then-else
  ;; TODO: Type of check definitely needs fixing.
  ([check : (U (Syntaxof grace:identifier) (Syntaxof grace:expression) (Syntaxof grace:member) (Syntaxof grace:method-call))]
   [tbody : (Listof (Syntaxof Any))]
   [ebody : (Listof (Syntaxof Any))])
  #:prefab)

(struct: grace:class-decl
  ([name       : (Syntaxof grace:identifier)]
   [param-name : (Syntaxof grace:identifier)]
   ;; TODO: Fix, maybe grace:identifier?
   [signature  : (Listof Any)]
   [body       : (Listof Any)])
  #:prefab)

;(struct: grace:code-seq
;  ([code : Any]))
(struct: grace:code-seq
  ([code : (Syntaxof (Listof (Syntaxof Any)))])
  #:prefab)

(struct: grace:newline
  ()
  #:prefab)

(provide (all-defined-out))
