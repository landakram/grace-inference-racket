#lang typed/racket

(require "astR.rkt")

;; Entry point for typechecking.
;; TODO: Fix return type.
(: typecheck ((Syntaxof grace:code-seq) -> Any))
(define (typecheck program)
  
  ;; TODO: Fix return
  (void))

(provide typecheck)
