#lang racket

(require racket/match
         "output.rkt"
         "parse.rkt")
(provide env-initial eval-with)

;; Evaluation toggles between eval and apply.

;This is adapted and modified from starter code provided at 
;http://matt.might.net/articles/implementing-a-programming-language/.
;A brief explanation of the code is there, although most of the article focuses
;on the lambda calculus.

;Note: methods that are copies of other ones but with C added to the name are
;more recent versions.
;They're designed to match the structure in genC.grace where a variable is bound
;and then hidden and two methods are created to read and modify that variable.

;Things to do:
;Make numbers, strings, etc. into objects with methods.
;Each object should have all methods listed in typechecker.grace

;Added Classes: classes are just an object with a method to return a new object.  
;Dealing with initargs and using the right environment is important.  
;Correct quasiquoting for object definition is tricky.

;Allow vars to be declared without being set to a value immediately 
;(Can set to void, but that doesn't really match Minigrace functionality.  
;If "var x" with no := is used in minigrace, then a method is called on x,
;an error is raised in minigrace but not necessarily in my version 
;(add error reporting!)

;insert for loops

;Easy: insert a place for objects to get def bindings.
;Methods should be def instead of var.

;include something to match apply (i.e. test #44)

;lists are difficult: almost everything I pass to ((eval exp env) ... )
;is in the format of Racket's lists, so matching to a list structure won't work.
;Need to create my own object, with list structure inside.

;First priority: include everything necessary to turn lexer.grace,
;compiler.grace ast.grace, and typecheck.grace into Racket-Grace

; eval dispatches on the type of expression:
(define (eval exp env)
  (match exp
    [(? symbol?)         (env-lookup env exp)]
    [(? number?)          exp] ;Replace this with a struct including the number.
    [(? boolean?)         exp] ;should never be called. I've made my own.
    [(? string?)          exp] ;Replace this with a struct including the string.
    [(? hash?)            exp] ;Envs are represented as hash tables
    ;with all the primitive methods, and cells for each def, var, and method
    [(? box?)             exp] ;Boxes are containers used to hold hash tables.
    ;They allow me to replace a hash with a new one and keep pointers into it.
    [`(myif ,ec ,et ,ef) (if 
                          (apply-proc (eval-send3 (eval ec env) '(bval) env) '())
                          (eval et env) 
                          (eval ef env))]
    [`(if ,ec ,et ,ef) (if (eval ec env) (eval et env) (eval ef env))]
    ;Forms of let take a list of pairs and a method to bind those pairs over.
    [`(letrec ,binds ,eb) (eval-letrec binds eb env)] 
    [`(let    ,binds ,eb) (eval-let    binds eb env)]
    [`(letC   ,binds ,eb) (eval-letC    binds eb env)]
    [`(lambda ,vs ,e)    `(closure ,exp ,env)]
    [`(set! ,v ,e)   (begin (match v
                              [(? symbol?) (env-set! env v (eval e env))]
                              [`(send2 ,obj ,meth) (eval `(set! ,meth ,e)
                                                         (eval obj env))]))]
    [`(send2 ,obj ,meth)   (eval-send3 obj meth env)]
    ;begin is needed because racket is not fond of "side-effects".  
    ;Our let is only designed to bind variables over one term, 
    ;while Grace wants them bound for the entire scope of the object.
    ;Solution is to have that one term be (begin (list (meth 1) (meth 2) etc.))
    [`(begin ,e1 ,e2)     (begin (eval e1 env)
                                 (eval e2 env))]
    [`(begin ,es)         (last (map (eval-with env) es))] 
    [`(while ,test ,body) (local [(define (loop)
                                    (if ((eval-with env) test)
                                        (begin (eval body env)
                                               (loop)) (void)))]
                            (loop))]
    [`(liststopairs ,first ,second) (changeliststopairs first second)]
    ;concatenate two strings together
    [`(++ ,e1 ,e2)    
     (let ([ex1 (eval e1 env)])
       (let ([ex2 (eval e2 env)])
         (match ex1
           [(? number?)  
            (match ex2
              [(? number?) 
               (string-append (number->string ex1) (number->string ex2))]
              [(? string?) (string-append (number->string ex1) ex2)])] 
           [(? string?)  
            (match ex2
              [(? number?) (string-append ex1 (number->string ex2))]
              [(? string?) (string-append ex1 ex2)])])))]
    ;versions of ==, !=, or, and and that match my booleans
    [`(== ,e1 ,e2) 
     (eval `(myif (equal? (eval ,e1 ,env) (eval ,e2 ,env)) (true) (false)) env)] 
    [`(!= ,e1 ,e2) 
     (eval `(myif (equal? (eval ,e1 ,env) (eval ,e2 ,env)) (false) (true)) env)] 
    [`(or ,e1 ,e2)  (eval `(myif (eval ,e1 ,env) (true) (eval ,e2 ,env)) env)]
    [`(and ,e1 ,e2)  (eval `(myif (eval ,e1 ,env) (eval ,e2 ,env) (false)) env)]
    ;different constructors for classes depending on #of arguments 
    [`(class ,constructor ,initargs ,methods ,body)
     (eval-newclassC constructor initargs methods body env)]
    ;different constructors for objects depending on # of arguments
    ;[`(object ,fields ,methods) (eval-newobj3 fields methods env)]
    [`(objectC ,fields ,methods ,body) (eval-newobjC fields methods body env)] 
    [`(initvar ,obj ,val) (eval-initvar obj val env)]
    [`(initvar* ,objs ,vals) (eval-initvar* objs vals env)]
    [`(initdef ,obj ,val) (eval-initdef obj val env)]
    [`(initdef* ,objs ,vals) (eval-initdef* objs vals env)]
    [`(,f . ,args)            (apply-proc
                               (eval f env) 
                               (map (eval-with env) args))]
    (void exp)))


; a handy wrapper for Currying eval:
(define (eval-with env) 
  (lambda (exp) (eval exp env)))

;This code needs work: the instance variables are not being properly passed in
;(define (eval-newclass constructor initargs fields methods env)
;  ;This is having trouble getting x to keep its value
;  (eval-newobj3 
;   '() 
;   (list (list constructor 
;               `(lambda (x) (let ((newinits 
;                                   (liststopairs ,initargs ((eval x ,env)))))
;                              (object newinits ,fields ,methods))))) env))

;(define (eval-newclass3 constructor initargs fields methods body env)
;  (eval-newobj3 '() (list (list constructor `(lambda (x) 
; (let ((newinits (liststopairs ,initargs ((eval x ,env))))) 
;(object newinits ,fields ,methods ,body))))) env)
;  )
;
;(define (eval-newclass2 constructor fields methods env)
;  (eval-newobj3 '() (list (list constructor 
;`(lambda () (object ,fields ,methods)))) env)
;  )

(define (eval-newclassC constructor initargs methods body env)
  ;(eval-newobjC (list) (list (list constructor 
  ;`(lambda (list) (object (list) ,methods ,body)))) (begin (list)) env))
  ;(eval-newobjC '() (list (list constructor `(lambda (x) (let ((newinits
  ;(liststopairs ,initargs ((eval (x) ,env))))) (objectC () ,methods ,body)))))
  ;'() env)
  (eval-newobjC 
   '() (list (list constructor 
                   `(lambda ,initargs (objectC ,initargs ,methods ,body)))) 
   '() env)
  )

;Used in class declarations. Takes two lists and returns a list of pairs.  
;Practical use is to take a list of instance variables and a list of arguments 
;and change them to a list of bindings that can then be used in a let statement
;or as part of an object's constructor.
(define (changeliststopairs first second)
  (match `(,first ,second)
    [`((,f . ,first) (,s . ,second))
     ;=>
     (cons (list f s) (changeliststopairs first second))]
    
    [`(() ())
     ;=>
     (list)]))


;preferred format for object declarations
(define (eval-newobjC fields methods body env)
  ;add bindings for outer and self
  (define env1 (box (env-empty)))
  (set-box! env1      
            (unbox (env-extend*
                    env1 (list 'outer) (list (env-contains env 'self)))))
  (set-box! env1    
            (unbox (env-extend* 
                    env1 (list 'self) (list (eval `(lambda () ,env1) env)))))
  (set-box! env1    (unbox (env-extend* env1 (list 'dynam) (list env))))
  ;(displayln env1)
  ;map all fields to false to avoid errors when they're accessed later
  ;(define fieldvars (map car fields))
  ;(define fieldexps (map cadr fields))
  ;(define falses    (map (lambda _ #f) fields))
  ;(set-box! env1    (unbox (eval-initvar* fieldvars falses env1)))
  ;(newline)
  ;(if (eq? fieldvars '(x)) 
  ;(displayln (env-lookup (env-lookup env1 'outer) 'z)) (displayln "here"))
  ;(displayln (env-lookup env1 'self))
  ;define and bind methods in standard letrec fashion, allowing mutual recursion
  (define methvars  (map car methods))
  (define methexps  (map cadr methods))
  (define ffs       (map (lambda _ #f) methods))
  (set-box! env1    (unbox (env-extend* env1 methvars ffs))) 
  ;(define env4      (env-extend* env1 methvars ffs))
  (define methvals  (map (eval-with env1) methexps))
  (env-set!* env1 methvars methvals)
  ;(set-box! env1 (unbox env4))
  ;(print fieldexps)
  ;(print (map (eval-with env4) fieldexps))
  ;now, with methods defined, bind to proper values for fields
  ;(define fieldvals (map (eval-with env4) fieldexps))
  ;(env-Cset!* env4 fieldvars fieldvals)
  ;(print fieldvals)
  ;then eval the body term
  ;(set-box! env1 (unbox env4))
  (eval body env1)
  ;(displayln env1)
  ;(env-remove* env1 fields)
  ;(displayln env1)
  ;finally, return that environment as the representation of the given object
  ;TODO: remove fields from objects?
  env1)

;Equivalent of dot-calling in Grace: 
;lets you look inside an object, and then call one of that object's methods.
(define (eval-send3 obj var env)
  (let* ((env*     ((eval-with env) obj)))
    (match var
      [(? symbol?)  (env-lookup env* var)]
      [`(,f . ,args)  
       (match obj
         [`(outer)
          (apply-proc (env-lookup env* f) (map (eval-with env) args))]
         [else 
          (apply-proc (env-contains env* f) (map (eval-with env) args))])])))

; eval for letrec:
(define (eval-letrec bindings body env)
  (let* ((vars (map car bindings))
         (exps (map cadr bindings))
         (fs   (map (lambda _ #f) bindings))
         (env* (env-extend* env vars fs))
         (vals (map (eval-with env*) exps)))
    (env-set!* env* vars vals)
    (eval body env*)))


;This is the preferred way to initialize var objects
;Makes a hidden var with the name $____, which should never be accessed by user
;(Lexer will not allow identifiers with $ included)
;Makes a method with same names as var that returns value of hiddenvar
;Makes a method with name __:= that sets hiddenvar to a value
(define (eval-initvar var val env)
  (let* ((hiddenvar (string->symbol (string-append "$" (symbol->string var))))
         (readmethod var)
         (modmethod (string->symbol (string-append (symbol->string var) ":="))) 
         (env0 (env-extend* env (list hiddenvar) (list ((eval-with env) val))))
         (env1 (env-extend* env0 (list readmethod) 
                            (list (eval `(lambda () ,hiddenvar) env0))))
         (env2 (env-extend* 
                env1 (list modmethod) 
                (list (eval `(lambda (b)
                               (begin (list (set! ,hiddenvar (b))))) env1)))))
    (set-box! env (unbox env2))
    env2))

;Initializes list of vars to list of initial values.
(define (eval-initvar* objs vals env)
  (match `(,objs ,vals)
    [`((,o . ,objs) (,v . ,vals))
     ;=>
     (eval-initvar* objs vals (eval-initvar o v env))]
    [`(() ())
     ; =>
     env]))

(define (eval-initdef var val env)
  (let* ((hiddenvar (string->symbol (string-append "$" (symbol->string var))))
         (readmethod var)
         (env0 (env-extend* env (list hiddenvar) (list ((eval-with env) val))))
         (env1 
          (env-extend* 
           env0 (list readmethod) (list (eval `(lambda () ,hiddenvar) env0)))))
    (set-box! env (unbox env1))
    env
    ))

;Initializes list of def to list of initial values.
(define (eval-initdef* objs vals env)
  (match `(,objs ,vals)
    [`((,o . ,obs) (,v . ,vls))
     ;=>
     (eval-initdef* obs vls (eval-initdef o v env))]        
    [`(() ())
     ; =>
     env]))

;(define (eval-enddef var env)
;  (let* ((hiddenvar (string->symbol (string-append "$" (symbol->string var))))
;         (readmethod var)
;         (env0 (env-remove* env (list hiddenvar)))
;         (env1 (env-remove* env0 (list readmethod))))
;    (set-box! env (unbox env1))
;    env1
;    ))
;
;(define (eval-enddef* objs env)
;  (match `(,objs)
;    [`((,o . ,objs))
;     ;=>
;     (eval-enddef* objs (eval-enddef o env))]        
;    [`(())
;     ; =>
;     env]))

;let a list of bindings be set over a body
(define (eval-letC bindings body env)
  (let* ((vars (map car bindings))
         (exps (map cadr bindings))
         (vals (map (eval-with env) exps))
         (env* (eval-initvar* vars vals env)))
    (eval body env*)))

(define (eval-let bindings body env)
  (let* ((vars (map car bindings))
         (exps (map cadr bindings))
         (vals (map (eval-with env) exps))
         (env* (env-extend* env vars vals)))
    (eval body env*)))

; applies a procedure to arguments:
(define (apply-proc f values)
  (match f
    [`(closure (lambda ,vs ,body) ,env) 
     ; =>
     (let ((env* (box (unbox env)))) 
       (eval-initdef* vs values env*)
       (let ((x (eval body env*)))
         x))]
    
    [`(primitive ,p)
     ; =>
     (let* ((val (apply p values)))
       (if (eq? val #t) 
           (begin (eval `(true) (env-initial))) 
           (if (eq? val #f) (eval `(false) (env-initial)) val)))]))

;; Environments map variables to mutable cells 
;; containing values.  Also have list of objects.
(define-struct cell ([value #:mutable]))

; empty environment:
(define (env-empty) (hash))

; initial environment, with bindings for primitives:
(define (env-initial)
  ;(env-extend* 
  (eval-newobjC 
   '() '() '(initdef* (true false)
                      ((send2 (boolean) (gtrue))(send2 (boolean) (gfalse))))
   (eval-initdef* 
    '(boolean) 
    '((objectC 
       () 
       ((new (lambda ( v) 
               (begin 
                 (list (objectC 
                        () ((not (lambda () 
                                   (begin (list 
                                           (if ((bval))
                                               (begin (list(new #f))) 
                                               (begin (list(new #t))))))))
                            (and (lambda (z) (if ((bval)) z (false))))
                            (asString (lambda () 
                                        (begin 
                                          (list (if ((bval))
                                                    
                                                    (begin (list "True"))
                                                    (begin (list "False"))))))))
                        (begin (list (initdef bval v)))))))))
       (begin (list (initdef gtrue (new #t))(initdef gfalse (new #f))))))
    (env-start)))
  ;next, extends environment further to add tru and fals objects
  ;naming is intentional to avoid any confusion with primitive 
  ;true and false but should be changeable without causing any issues.
  ;The only thing users should see is the asString method
  ;so it may be fine as-is.
  ;   '(tru fals 
  ;         ;<
  ;         )
  ;   (let* ((env0 (env-extend* (box (env-empty)) '(asString) `("true"))) 
  ;          (env1 (env-extend* (box (env-empty)) '(asString) `("false")))) 
  ;     (begin (set-box! env0 (unbox (env-extend* env0 '(not prefix!) 
  ;`((closure (lambda () ,env1) ,env1) (closure (lambda () ,env1) ,env1)))))
  ;            (set-box! env1 (unbox (env-extend* env1 '(not prefix!) 
  ;`((closure (lambda () ,env0) ,env0) (closure (lambda () ,env0) ,env0)))))
  ;            `(,env0 ,env1))))
  )

(define (env-contains env var)
  (if (primitive? var) ((lambda (s) (list 'primitive s)) var)
      (if (hash-has-key? (unbox env) var)
          (match (hash-ref (unbox env) var)
            [(? cell?)  
             (cell-value (hash-ref (unbox env) var))]
            [x
             (hash-ref (unbox env) var)])
          (void))))


; looks up a value:
(define (env-lookup env var)
  (if (primitive? var) ((lambda (s) (list 'primitive s)) var)
      (if (hash-has-key? (unbox env) var)
          (begin 
            ;(displayln var)
            ;(displayln env)
            (match (hash-ref (unbox env) var)
              [(? cell?)  
               (cell-value (hash-ref (unbox env) var))]
              [x
               (hash-ref (unbox env) var)]))
          (if (hash-has-key? (unbox env) 'dynam)                      
              (let ((temp (env-lookup env 'dynam)))                              
                (env-lookup temp var)) 
              (begin (print "there") env)))))


; sets a value in an environment:
(define (env-set! env var value)
  (set-cell-value! (hash-ref (unbox env) var) value))    

; extends an environment with several bindings:
(define (env-extend* env varbls values)
  (match `(,varbls ,values)
    [`((,v . ,varbls) (,val . ,values))
     ; =>
     (env-extend* (box (hash-set (unbox env) v (make-cell val))) varbls values)]
    [`(() ())
     ; =>
     env]))

;(define (env-remove* env varbls)
;  (match `(,varbls)
;    [`((,v . ,varbls))
;     ; =>
;     ;(displayln v)
;     (set-box! env (hash-remove (unbox env) v))
;     (env-remove* env varbls)]
;    [`(())
;     ; =>
;     env]))


;extends environment, but instead of mapping to a mutable cell containing 
;the value, just maps it directly to the value
;eventually, set! method will change, and then we should change this so it
;looks like initvar, but without the var:= method.

;TODO: consider removing this.
;(define (env-extdef* env invarbls values)
;  (match `(,invarbls ,values)
;    [`((,i . ,invarbls) (,val . ,values))
;     ; =>
;     (set-box! env (box (env-extend* (hash-set (unbox env) i val)
;                                     invarbls values)))]
;    [`(() ())
;     ; =>
;     env]))

;;extend an environment mapping a list of values to void.
;;Doesn't entirely match grace behavior
;;racket will allow you to call something like print on void
;;while grace sends an error if you try to call a method on an unitialized var.
;(define (env-extempty env varbls)
;  (match `(,varbls)
;    [`((,v . ,varbls))
;     ; =>
;     (set-box! env (box (env-extempty (hash-set (unbox env) v 
;                                                (make-cell (void))) varbls)))]
;    
;    [`(())
;     ; =>
;     env]))


; mutates an environment with several assignments:
(define (env-set!* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     ; =>
     (begin
       (env-set! env v val)
       (env-set!* env vars values))]
    
    [`(() ())
     ; =>
     (void)]))

(define (env-start) 
  (env-extend*
   (box (env-empty))
   ;Takes a whole list of primitives and binds them to racket equivalents
   ;many of these will need to be replaced:
   ;all the math ones will need to extract values out of new number objects
   ;and then call primitive version rather than being in current form
   '(+  -  /  *  %   <= >= < > eq? equal? void  display newline string-append
        cons list eval eval-with expt false? number->string null list? ==
        print)
   (map (lambda (s) (list 'primitive s))
        `(,+ ,- ,/ ,* ,modulo ,<= ,>= ,< ,> ,eq? ,equal? ,void ,display
             ,newline ,string-append ,cons ,list ,eval ,eval-with ,expt
             ,false? ,number->string ,null ,list? ,equal? 
             ,(lambda (x) (match x 
                            ;check if x has asString defined, and call that
                            ;ideally, all objects will have asString defined
                            [(? box?) (if (hash-has-key? (unbox x) 'asString)
                                          (begin 
                                            (display 
                                             (apply-proc 
                                              (eval `asString x) '()))
                                            (newline))
                                          (begin (display x) (newline)))]
                            [any (begin (display x) (newline))]))))))


;; Evaluation tests.

; define new syntax to make tests look prettier:
(define-syntax 
  test-eval 
  (syntax-rules (====)
    [(_ program ==== value)
     (let ((result (eval (quote program) (env-initial))))
       (when (not (equal? program value))
         (error "test failed!")))]))

;(test-eval
;  ((lambda (x) (+ 3 4)) 20)
;  ====
;  7)

;Commented out because it relies on <=, which returns a racket #t or #f.  
;Trying to phase that out, replace with one that returns my tru or false.
;(test-eval
;  (letrec ((f (lambda (n) 
;                 (if (<= n 1)
;                     1
;                     (* n (f (- n 1)))))))
;     (f 5))
;  ====
;  120)

;(test-eval
;  (let ((x 100))
;    (begin
;      (set! x 20)
;      x))
;  ====
;  20)

;(test-eval
;  (let ((x 1000))
;    (begin (let ((x 10))
;             20)
;           x))
;  ====
;  1000)


;; Programs are translated into a single letrec expression.

(define (define->binding define)
  (match define
    [`(define (,f . ,formals) ,body)
     ; =>
     `(,f (lambda ,formals ,body))]
    
    [`(define ,v ,value)
     ; =>
     `(,v ,value)]
    
    [else 
     ; =>
     `(,(gensym) ,define)]))

(define (transform-top-level defines)
  `(letrec ,(map define->binding defines)
     (void)))

(define (eval-program program)
  (eval (transform-top-level program) (env-initial)))

(define (read-all)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-all)))))

(define (p in) (parse (object-name in) in))

;(define inp (open-input-file 
;"/home/ryannow/BRUCE-TOTRANSFER2/donetests/t016_objectmeth_test.grace"))
;(define a (p inp))
;(display (AST-to-RG (syntax-e a)))
(define a (p (open-input-string "object{
print(1)
}
"
                                )))
;(define-values (in out) (make-pipe))
;(display (AST-to-RG (syntax-e a)))
;(display (AST-to-RG (syntax-e a)) out)
;(let* ((temp (read in))
;       (test (print (cdr temp)))
; (out (eval temp (env-initial))))
; (void))


; read in a program, and evaluate:
;(eval-program (read-all))

