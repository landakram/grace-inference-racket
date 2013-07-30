#lang racket

(struct MethodType 
  (name
   signature 
   rtype) 
  #:prefab)

(struct grace:type-annot 
  (value) 
  #:prefab)

(struct grace:number
  (value)
  #:prefab)

(struct grace:str
  (value)
  #:prefab)

(struct grace:identifier
  (value
   type)
  #:prefab)

(struct grace:method-def
  (name
   signature
   ;[signature : (Listof IdentifierType)]
   ;[rtype     : IdentifierType])
   rtype)
  #:prefab)

(struct grace:type-def
  (name
   methods)
  ;[methods : (Listof (Syntaxof grace:method-def))])
  #:prefab)

(struct grace:var-decl
  (name
   type
   value)
  #:prefab)

(struct grace:def-decl
  (name
   type
   ;; TODO: possibly fix type of value
   value)
  #:prefab)

(struct grace:bind
  (name
   ;; TODO: possibly fix type of value
   value)
  #:prefab)

(struct grace:expression
  (op
   ;; TODO: Fix type of operands
   lhs
   rhs)
  #:prefab)

(struct grace:method-call
  (name
   ;; TODO: Fix type of args
   args)
  #:prefab)

(struct grace:object
  (body)
  #:prefab)

(struct grace:method
  (name     
   signature
   ;; TODO: Fix type of body
   ;[body : (Listof (Syntaxof Any))]
   body
   rtype)
  #:prefab)

(struct grace:member
  (parent 
   name)   
  #:prefab)

(struct grace:return
  ;; TODO: Fix type of value, maybe grace:expression or identifier.
  (value)
  #:prefab)

(struct grace:if-then-else
  ;; TODO: Type of check definitely needs fixing.
  (check
   ;[tbody : (Listof (Syntaxof Any))]
   ;[ebody : (Listof (Syntaxof Any))])
   tbody
   ebody)
  #:prefab)

(struct grace:class-decl
  (name
   param-name
   ;; TODO: Fix, maybe grace:identifier?
   signature
   body)
  #:prefab)

;(struct grace:code-seq
;  ([code : Any]))
(struct grace:code-seq
  (code)
  #:prefab)

(struct grace:newline
  ()
  #:prefab)

(provide (all-defined-out))
