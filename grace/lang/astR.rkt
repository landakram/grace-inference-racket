#lang typed/racket

(struct: method-type
  ([name : String]
   [signature : (Listof String)]
   [rtype : String]))

(define-type grace-type 
  (U (Syntaxof grace:identifier) #f))

(struct: grace:number
  ([value : Number]))

(struct: grace:str
  ([value : String]))

(struct: grace:identifier
  ([value : String]
   [type  : grace-type]))

(struct: grace:method-def
  ([name      : (Syntaxof grace:identifier)]
   [signature : (Listof (Syntaxof grace:identifier))]
   [rtype     : (Syntaxof grace:identifier)]))

(struct: grace:type-def
  ([name    : (Syntaxof grace:identifier)]
   [methods : (Listof grace:method-def)]))

(struct: grace:var-decl
  ([name  : (Syntaxof grace:identifier)]
   [type  : grace-type]
   [value : Any]))

(struct: grace:def-decl
  ([name  : (Syntaxof grace:identifier)]
   [type  : grace-type]
   ;; TODO: possibly fix type of value
   [value : Any]))

(struct: grace:bind
  ([name  : (Syntaxof grace:identifier)]
   ;; TODO: possibly fix type of value
   [value : Any]))

(struct: grace:expression
  ([op  : Symbol]
   ;; TODO: Fix type of operands
   [lhs : Any]
   [rhs : Any]))

(struct: grace:method-call
  ([name : (U (Syntaxof grace:identifier) (Syntaxof grace:member))]
   ;; TODO: Fix type of args
   [args : (Listof Any)]))

(struct: grace:object
  ([body : (Listof (Syntaxof Any))]))

(struct: grace:method
  ([name      : (Syntaxof grace:identifier)]
   [signature : (Listof (Syntaxof grace:identifier))]
   ;; TODO: Fix type of body
   [body : (Listof (Syntaxof Any))]
   [type : grace-type]))

(struct: grace:member
  ([parent : (U (Syntaxof grace:identifier) (Syntaxof grace:member))]
   [name   : (Syntaxof grace:identifier)]))

(struct: grace:return
  ;; TODO: Fix type of value, maybe grace:expression or identifier.
  ([value : Any]))

(struct: grace:if-then-else
  ;; TODO: Might need fixing.
  ([check : (Syntaxof grace:expression)]
   [tbody : (Listof (Syntaxof Any))]
   [ebody : (Listof (Syntaxof Any))]))

(struct: grace:class-decl
  ([name       : (Syntaxof grace:identifier)]
   [param-name : (Syntaxof grace:identifier)]
   ;; TODO: Fix, maybe grace:identifier?
   [signature  : (Listof Any)]
   [body       : (Listof Any)]))

;(struct: grace:code-seq
;  ([code : Any]))
(struct: grace:code-seq
  ([code : (Listof (Syntaxof Any))]))

(struct: grace:newline
  ())

(provide (all-defined-out))
