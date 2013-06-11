#lang racket
(require "parse.rkt"
         "ast.rkt"
         "typecheck.rkt"
         parser-tools/lex)

(provide read-syntax
         read
         get-info)

;; Parses the grace syntax for a module
;; 
;; Without a backend for Grace, this function simply creates
;; a fresh racket module, requires all grace AST structures, 
;; and provides the root of the parsed AST.
(define (read-syntax src-name in)
  (let* ([p-name (object-name in)]
         (stx (parse src-name in))
         [name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol
                      (path->string (path-replace-suffix name #""))))
                   'anonymous)])
    (define datum (syntax->datum stx))
    (define env (typecheck stx))
    ;(display (list? env))
    (datum->syntax #f `(module ,name racket 
                         (provide st env)
                         (require grace/lang/ast)
                         (require grace/lang/typecheck)
                         (define st ,datum)
                         (define env (list ,@env))))))

(define (p in) (parse (object-name in) in))

;; In case `read' is used, instead of `read-syntax':
(define (read in)
  (syntax->datum (read-syntax (object-name in) in)))

;; To get info about the language's environment support:
(define (get-info in mod line col pos)
  (lambda (key default) 
    (case key 
      [(drracket:toolbar-buttons)
       (dynamic-require 'grace/tools/drracket-buttons 'drracket-buttons)]
      [else default])))