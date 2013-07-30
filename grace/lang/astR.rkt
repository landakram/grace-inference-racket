#lang typed/racket

;;; TODO: Account for empty list '() that is inserted in the parser, either by
;;;   adding them to the acceptable types below or changing the use of empty.

(struct: MethodType
  ([name : String]
   [signature : (Listof String)]
   [rtype : String])
  #:prefab)

(define-type IdentifierType
  (Syntaxof grace:identifier))

(define-type BodyType
  (Syntaxof (Listof (Syntaxof Any))))

(define-type SignatureType
  (Syntaxof (Listof IdentifierType)))

(define-type TypeType
  ;(U IdentifierType #f))
  ;IdentifierType)
  ;(Syntaxof String))
  (Syntaxof grace:type-annot))

(struct: grace:type-annot
  ([value : String])
  #:prefab)

(struct: grace:number
  ([value : (Syntaxof Number)])
  #:prefab)

(struct: grace:str
  ([value : (Syntaxof String)])
  #:prefab)

(struct: grace:identifier
  ([value : String]
   [type  : TypeType])
  #:prefab)

(struct: grace:method-def
  ([name      : IdentifierType]
   [signature : SignatureType]
   ;[signature : (Listof IdentifierType)]
   ;[rtype     : IdentifierType])
   [rtype : TypeType])
  #:prefab)

(struct: grace:type-def
  ([name    : IdentifierType]
   [methods : (Syntaxof (Listof (Syntaxof grace:method-def)))])
   ;[methods : (Listof (Syntaxof grace:method-def))])
  #:prefab)

(struct: grace:var-decl
  ([name  : IdentifierType]
   [type  : TypeType]
   [value : (Syntaxof Any)])
  #:prefab)

(struct: grace:def-decl
  ([name  : IdentifierType]
   [type  : TypeType]
   ;; TODO: possibly fix type of value
   [value : (Syntaxof Any)])
  #:prefab)

(struct: grace:bind
  ([name  : (U (Syntaxof grace:member) IdentifierType)]
   ;; TODO: possibly fix type of value
   [value : (Syntaxof Any)])
  #:prefab)

(struct: grace:expression
  ([op  : Symbol]
   ;; TODO: Fix type of operands
   [lhs : (Syntaxof Any)]
   [rhs : (Syntaxof Any)])
  #:prefab)

(struct: grace:method-call
  ([name : (U IdentifierType (Syntaxof grace:member))]
   ;; TODO: Fix type of args
   [args : (Listof (Syntaxof Any))])
  #:prefab)

(struct: grace:object
  ([body : BodyType])
  #:prefab)

(struct: grace:method
  ([name      : IdentifierType]
   [signature : SignatureType]
   ;; TODO: Fix type of body
   ;[body : (Listof (Syntaxof Any))]
   [body : BodyType]
   [rtype : TypeType])
  #:prefab)

(struct: grace:member
  ([parent : (U IdentifierType (Syntaxof grace:member))]
   [name   : IdentifierType])
  #:prefab)

(struct: grace:return
  ;; TODO: Fix type of value, maybe grace:expression or identifier.
  ([value : Any])
  #:prefab)

(struct: grace:if-then-else
  ;; TODO: Type of check definitely needs fixing.
  ([check : (U IdentifierType (Syntaxof grace:expression) (Syntaxof grace:member) (Syntaxof grace:method-call))]
   ;[tbody : (Listof (Syntaxof Any))]
   ;[ebody : (Listof (Syntaxof Any))])
   ;[tbody : BodyType]
   [tbody : (Syntaxof (Listof (Syntaxof Any)))]
   [ebody : BodyType])
  #:prefab)

(struct: grace:class-decl
  ([name       : IdentifierType]
   [param-name : IdentifierType]
   ;; TODO: Fix, maybe grace:identifier?
   [signature  : SignatureType]
   [body       : BodyType])
  #:prefab)

;(struct: grace:code-seq
;  ([code : Any]))
(struct: grace:code-seq
  ([code : BodyType])
  #:prefab)

(struct: grace:newline
  ()
  #:prefab)

(provide (all-defined-out))
