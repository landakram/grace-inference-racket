#lang typed/racket

(require "ast.rkt"
         "parseR.rkt")

;; The syntax currently being resolved
(define stx
  (make-parameter #f))

;; Raise a typechecking error and formats the message.
;(define: (tc-error [msg : String] . [rest : String *]) : Any
(: tc-error (String String * -> Any))
(define (tc-error msg . rest)
  (raise-syntax-error 'typecheck (apply format msg rest) (stx)))
