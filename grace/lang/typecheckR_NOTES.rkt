#lang typed/racket

;(require "ast.rkt"
;         "parseR.rkt")

;(require/typed "ast.rkt"
;               [grace:code-seq-code ((Syntaxof (Listof Any)) -> (Listof (Syntaxof Any)))])

;(struct: method-type
;  ([name : String]
;   [signature : (Listof String)]
;   [rtype : String]))


(require "astR.rkt")

(define-type method-list
  (Listof method-type))

(define-type scope-type-defs
  (HashTable String (Listof method-list)))

(define-type scope-type-env
  (HashTable String String))

(: current-type-defs scope-type-defs)
(define current-type-defs
  (make-hash))

(: current-type-env scope-type-env)
(define current-type-env
  (make-hash))

(: type-defs (Listof scope-type-defs))
(define type-defs
  (list current-type-defs))

(: type-env (Listof scope-type-env))
(define type-env
  (list current-type-env))

(: get-type-defs (Syntax -> (Listof scope-type-defs)))
(define (get-type-defs stx)
  (list))

(: build-type-env (Syntax -> (Listof scope-type-env)))
(define (build-type-env stx)
  (list))

;; TODO: Fix return type
(: typecheck (Syntax -> Any))
(define (typecheck stx)
  (let* ([type-defs (get-type-defs stx)]
         [type-env (build-type-env stx)])
    
    ;; TODO: implement
    (void)))

;; Entry program
;; TODO: Fix return
;(: typecheck-prog ((Syntaxof grace:code-seq) -> (Listof Any)))
(: typecheck-prog ((Syntaxof grace:code-seq) -> Any))
(define (typecheck-prog prog)
;  (syntax->list (grace:code-seq-code (syntax-e prog)))
  (grace:code-seq-code (syntax-e prog))
  
  ;; TODO: Fix return
  (void))

#|
Notes:
- In the type-env (with identifiers) may need to keep track of which are defs and which are vars.
- As noted below, probably need an entry function and helper.

## EXAMPLE PSEUDO_CODE ##
function typecheck
  build-environment() # This skips any inner blocks and just looks at the outermost scope
  look-for-errors() # This will check the RHS of assignments, make sure no type errors, etc...

function build-environment
  push(typedef)to(scope-type-defs)
  push(identifier)to(scope-type-env)

function look-for-errors
  match/case
    var-decl ...
    def-decl ...

    ....

    object-decl -> typecheck()  # This now recursively jumps into the block pushing a new env on the stack
    if-then-else -> typecheck() # Similarly for other constructs that create a new scope
## END PSEUDO_CODE ##


MAYBE: (first sketch)
* Need an entry function typecheck, then a recursive typecheck-helper.
* The entry function will deal with the prelude and maybe all the above code needs to go into there.
- It then should call some recursive helper that will first get the typedefs and build the type-env
    for the outermost scope and then typecheck it, since nothing in a more inner scope should affect
    the outermost scope. Then, the typechecker can recursively enter inner-more scopes, building on
    top of the stack of typedefs and type-envs.
- It should do this by running three functions: one will go through the list of syntax objects and
    pick out the type-definitions and add it to the typedefs. The second will go through and look
    for identifier bindings and add it to type-env. The last will 


MAYBE MAYBE: (0.5th sketch)
- The ast may have to be changed so that (void) isn't given in the list, as the typecheck function
    may be expecting a Syntax type. We could add a newline struct in ast.rkt, or we could just
    insert an empty Syntax object.
|#















;; TODO: Possibly Remove

;; The syntax currently being resolved
(: stx (Parameterof Syntax))
(define stx
  (make-parameter (datum->syntax #f (list))))

;; Raise a typechecking error and formats the message.
;(define: (tc-error [msg : String] . [rest : String *]) : Any
(: tc-error (String String * -> Any))
(define (tc-error msg . rest)
  (raise-syntax-error 'typecheck (apply format msg rest) (stx)))


(struct: type-node-type
  ([name : String]
   [methods : (Listof method-type)]
   [subtype : (type-node-type -> Boolean)]))
