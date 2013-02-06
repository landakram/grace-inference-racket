#lang racket
(require "parse.rkt"
         syntax/strip-context
         parser-tools/lex)

(provide read-syntax
         read
         get-info)

;; To read a module:
(define (read-syntax src-name in)
  (let* ([p-name (object-name in)]
         (stx (parse src-name in))
         [name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol
                      (path->string (path-replace-suffix name #""))))
                   'anonymous)])
    (display (syntax->datum stx))
    (datum->syntax #f `(module ,name racket 
                         (provide data)
                         (struct var-decl (name value))
                         (struct def-decl (name value))
                         (struct bind (name value))
                         (struct num-exp (n))
                         (struct var-exp (i))
                         (struct arith-exp (op e1 e2))
                         (struct method-call (name args))
                         (struct object-node (body))
                         (struct method (name body))
                         (struct member (parent name))
                         (struct code-seq (c))
                         (struct str (s))
                         (define data ,(syntax->datum stx))))))

;; In case `read' is used, instead of `read-syntax':
(define (read in)
  (syntax->datum (read-syntax (object-name in) in)))

;; To get info about the language's environment support:
(define (get-info in mod line col pos)
  (lambda (key default) default))