#lang racket

(require racket/match)

;; Evaluation toggles between eval and apply.

;This is adapted and modified from starter code provided at 
;http://matt.might.net/articles/implementing-a-programming-language/.
;A brief explanation of the code is there, although most of the article focuses on the lambda calculus.

;Note: methods that are copies of other ones but with C added to the name are more recent versions.
;They're designed to match the structure in genC.grace where a variable is bound and then hidden
;and two methods are created to read and modify that variable.

;Things to do:
;Make numbers, strings, etc. into objects with methods.  Each object should have all methods listed in typechecker.grace

;Add defs with same initargs structure

;Added boolean tru and fals objects 
;(naming is intentional to allow me to distinguish from true and false that Racket recognizes, 
;but so far there have been no issues with Racket reading mine as theirs or vice versa.)
;Now need to change all inherited primitive methods so instead of returning #t and #f they return my tru and fals

;Add Classes: classes are just an object with a method that returns a new object.  
;Dealing with initargs and using the right environment at the right time will be important.  
;Correct quasiquoting for object definition is tricky.

;Allow vars to be declared without being set to a value immediately 
;(Can set to void, but that doesn't really match Minigrace functionality.  
;If "var x" with no := is used in minigrace, then a method is called on x,
;an error is raised in minigrace but not necessarily in my version (add error reporting!)

;insert for loops

;Easy: insert a place for objects to get def bindings.  Methods should be def instead of var.

;include something to match apply (i.e. test #44)

;lists are difficult: almost everything I pass to ((eval exp env) ... ) is in the format of Racket's lists, so matching to a list structure won't work.
;Need to create my own object, with list structure inside.

;First priority: include everything necessary to turn lexer.grace, compiler.grace ast.grace, and typecheck.grace into Racket-Grace

;;Consider making everything return a pair of return value and environment.  Every step would then read in the value returned
;;and the environment, change the

; eval dispatches on the type of expression:
(define (eval exp env)
  (match exp
    [(? symbol?)         (env-lookup env exp)]
    [(? number?)          exp] ;Replace this with a new structure that includes the number
    [(? boolean?)         (print "bool") exp] ;should never be called because I've created my own tru and fals to replace racket #t and #f.  
    [(? string?)          exp] ;Replace this with a new structure that holds the string
    [(? hash?)            exp] ;Objects are represented as hash tables with all the primitive methods, and cells for each def, var, and method
    [(? box?)             exp] ;Boxes are containers used to hold hash tables.  
    ;They allow me to replace an old immutable hash with a new one but have all the old pointers in still work.
    ;[`(if ,ec ,et ,ef)    (if (eval ec env)
    ;                          (eval et env)
    ;                          (eval ef env))]
    [`(myif ,ec ,et ,ef) (if (eq? (eval ec env) (eval 'fals env)) ;This compares for equality between the condition and my fals object.
                             (eval ef env) ;Replaces original if above, which reads in Racket's #t and #f.
                             (eval et env))];Keeping old version available because Racket primitives will still return #t and #f.
                              
    [`(letrec ,binds ,eb) (eval-letrec binds eb env)] ;various forms of let take a list of pairs and then a single method to bind those pairs over
    [`(let    ,binds ,eb) (eval-let    binds eb env)]
    [`(letC   ,binds ,eb) (eval-letC    binds eb env)]
    [`(init   ,vars) (env-extempty env vars)] 
    [`(def    ,binds ,eb) (eval-def    binds eb env)]
    [`(lambda ,vs ,e)    `(closure ,exp ,env)]
    ;These shouldn't be here: current code will print ASTs that uses this : instead, it should call the "{var}:=" method to allow overriding to work properly
    [`(set! ,v ,e)        (match v
                            [(? symbol?) (env-set! env v (eval e env))]
                            [`(send2 ,obj ,meth) (eval `(set! ,meth ,e) (eval obj env))])]
    [`(setC! ,v ,e)       (match v
                            [(? symbol?) (env-Cset! env v (eval e env))]
                            [`(send2 ,obj ,meth) (eval `(setC! ,meth ,e) (eval obj env))])]
    
    [`(send2 ,obj ,meth)   (eval-send3 obj meth env)]
    ;begin is needed because racket is not fond of "side-effects".  
    ;Our let is only designed to bind variables over one term, while Grace wants them bound for the entire scope of the object.
    ;Solution is to have that one term be (begin (list (method 1) (method 2) etc.))
    [`(begin ,e1 ,e2)     (begin (eval e1 env)
                                 (eval e2 env))]
    [`(begin ,es)         (map (eval-with env) es)] 
    [`(while ,test ,body) (local [(define (loop)
                            (if ((eval-with env) test)
                              (begin (eval body env)
                              (loop)) (void)))]
                               (loop))]
    [`(liststopairs ,first ,second) (changeliststopairs first second)]
    ;concatenate two strings together
    [`(++ ,e1 ,e2)    (let ([ex1 (eval e1 env)])
                            (let ([ex2 (eval e2 env)])
                              (match ex1
                                [(? number?)  (match ex2
                                                [(? number?) (string-append (number->string ex1) (number->string ex2))]
                                                [(? string?) (string-append (number->string ex1) ex2)])]                                               
                                [(? string?)  (match ex2
                                                [(? number?) (string-append ex1 (number->string ex2))]
                                                [(? string?) (string-append ex1 ex2)])])))]
    ;versions of ==, !=, or, and and that match my booleans
    [`(== ,e1 ,e2)     (if (equal? (eval e1 env) (eval e2 env)) (eval 'tru env) (eval 'fals env))] 
    [`(!= ,e1 ,e2)     (if (equal? (eval e1 env) (eval e2 env)) (eval 'fals env) (eval 'tru env))]
    ;[`(or ,e1 ,e2)     (or (eval e1 env) (eval e2 env))]
    [`(myor ,e1 ,e2)   (if (eq? e1 'fals) (if (eq? e2 'fals) (eval 'fals env) (eval 'tru env)) (eval 'tru env))]  
    ;[`(and ,e1 ,e2)    (and (eval e1 env) (eval e2 env))]
    [`(myand ,e1 ,e2)   (if (eq? e1 'fals) (eval 'fals env) (if (eq? e2 'fals) (eval 'fals env) (eval 'tru env)))]
    
;    [`(object ,initargs ,fields ,methods) (eval-newobj2 initargs fields methods env)]
    ;different constructors for classes depending on how many arguments you want to send
    [`(class ,constructor ,initargs ,fields ,methods) (eval-newclass constructor initargs fields methods env)]
    [`(class ,constructor ,initargs ,fields ,methods ,body) (eval-newclass3 constructor initargs fields methods body env)]
    [`(class ,constructor ,fields ,methods) (eval-newclass2 constructor fields methods env)]
    ;different constructors for objects depending on how many arguments you want to send
    [`(object ,initargs ,fields ,methods ,body) (eval-newobj initargs fields methods body env)]
    ;[`(object ,initargs ,fields ,methods) (eval-newobj2 initargs fields methods env)]
    [`(object ,fields ,methods) (eval-newobj3 fields methods env)]
    [`(object ,fields ,methods ,body) (eval-newobj4 fields methods body env)]
    [`(objectC ,fields ,methods ,body) (eval-newobjC fields methods body env)]    
    [`(initvar ,obj ,val) (eval-initvar obj val env)]
    [`(initvar* ,objs ,vals) (eval-initvar* objs vals env)]


    [`(,f . ,args)         (apply-proc
                           (eval f env) 
                           (map (eval-with env) args))]))


; a handy wrapper for Currying eval:
(define (eval-with env) 
  (lambda (exp) (eval exp env)))

;This code needs work: the instance variables are not being properly passed in
(define (eval-newclass constructor initargs fields methods env)
   ;This is having trouble getting x to keep its value
 (eval-newobj3 '() (list (list constructor `(lambda (x) 
                                                   (let ((newinits (liststopairs ,initargs ((eval x ,env))))) (object newinits ,fields ,methods))))) env)
  )

(define (eval-newclass3 constructor initargs fields methods body env)
 (eval-newobj3 '() (list (list constructor `(lambda (x) 
                                                   (let ((newinits (liststopairs ,initargs ((eval x ,env))))) (object newinits ,fields ,methods ,body))))) env)
  )

(define (eval-newclass2 constructor fields methods env)
  (eval-newobj3 '() (list (list constructor `(lambda () (object ,fields ,methods)))) env)
  )


;An internal method used in class declarations.  Takes two lists and returns a list of pairs.  
;Practical use is to take a list of instance variables and a list of arguments 
;and change them to a list of bindings that can then be used in a let statement or as part of an object's constructor.
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
  (define env1      (env-extend* env (list 'outer) (list (eval `(lambda () ,env) env))))
  (set-box! env1    (unbox (env-extend* env1 (list 'self) (list env1))))
  ;map all fields to false to avoid errors when they're accessed later
  (define fieldvars (map car fields))
  (define fieldexps (map cadr fields))
  (define falses    (map (lambda _ #f) fields))
  (set-box! env1    (unbox (eval-initvar* fieldvars falses env1)))
  ;define and bind methods in standard letrec fashion, allowing mutual recursion
  (define methvars  (map car methods))
  (define methexps  (map cadr methods))
  (define ffs       (map (lambda _ #f) methods))
  (define env4      (env-extend* env1 methvars ffs))
  (define methvals  (map (eval-with env4) methexps))
  (env-set!* env4 methvars methvals)
  (set-box! env1 (unbox env4))
  ;(print fieldexps)
  ;(print (map (eval-with env4) fieldexps))
  ;now, with methods defined, get proper values for fields and bind to those values
  (define fieldvals (map (eval-with env4) fieldexps))
  (env-Cset!* env4 fieldvars fieldvals)
  ;(print fieldvals)
  ;then eval the body term
  (eval body env4)
  ;finally, return that environment as the representation of the given object
  env4
  )

;older versions of objects: examples of how different parts can be excluded
;or how initialization arguments can be added (treated similarly to fields that are just bound first, though).
(define (eval-newobj initargs fields methods body env)
  (define initvars  (map car (eval initargs env)))
  (define initexps  (map cadr (eval initargs env)))
  (define fs        (map (lambda _ #f) (eval initargs env)))
  (define env*      (env-extend* env initvars fs))
  (define initvals  (map (eval-with env*) initexps))
  (env-set!* env* initvars initvals)
  (define fieldvars (map car fields))
  (define fieldexps (map cadr fields))
  (define falses    (map (lambda _ #f) fields))
  (define env2      (env-extend* env* fieldvars falses))
  (define methvars  (map car methods))
  (define methexps  (map cadr methods))
  (define ffs       (map (lambda _ #f) methods))
  (define env4      (env-extend* env2 methvars ffs))
  (define methvals  (map (eval-with env4) methexps))
  (env-set!* env4 methvars methvals)
  (define fieldvals (map (eval-with env4) fieldexps))
  (env-set!* env4 fieldvars fieldvals)
  (eval body env4)
  env4
  )

(define (eval-newobj2 initargs fields methods env)
  (define initvars  (map car (eval initargs env)))
  (define initexps  (map cadr (eval initargs env)))
  (define fs        (map (lambda _ #f) (eval initargs env)))
  (define env*      (env-extend* env initvars fs))
  (define initvals  (map (eval-with env*) initexps))
  (env-set!* env* initvars initvals)
  (define fieldvars (map car fields))
  (define fieldexps (map cadr fields))
  (define falses    (map (lambda _ #f) fields))
  (define env2      (env-extend* env* fieldvars falses))
  (define methvars  (map car methods))
  (define methexps  (map cadr methods))
  (define ffs       (map (lambda _ #f) methods))
  (define env4      (env-extend* env2 methvars ffs))
  (define methvals  (map (eval-with env4) methexps))
  (env-set!* env4 methvars methvals)
  (define fieldvals (map (eval-with env4) fieldexps))
  (env-set!* env4 fieldvars fieldvals)
  env4
  )

(define (eval-newobj3 fields methods env)
  (define fieldvars (map car fields))
  (define fieldexps (map cadr fields))
  (define falses    (map (lambda _ #f) fields))
  (define env2      (env-extend* env fieldvars falses))
  (define methvars  (map car methods))
  (define methexps  (map cadr methods))
  (define ffs       (map (lambda _ #f) methods))
  (define env4      (env-extend* env2 methvars ffs))
  (define methvals  (map (eval-with env4) methexps))
  (env-set!* env4 methvars methvals)
  (define fieldvals (map (eval-with env4) fieldexps))
  (env-set!* env4 fieldvars fieldvals)
  env4
  )

(define (eval-newobj4 fields methods body env)
  (define env1      (env-extend* env (list 'outer) (list env)))
  (define fieldvars (map car fields))
  (define fieldexps (map cadr fields))
  (define falses    (map (lambda _ #f) fields))
  (define env2      (env-extend* env1 fieldvars falses))
  (define methvars  (map car methods))
  (define methexps  (map cadr methods))
  (define ffs       (map (lambda _ #f) methods))
  (define env4      (env-extend* env2 methvars ffs))
  (define methvals  (map (eval-with env4) methexps))
  (env-set!* env4 methvars methvals)
  (define fieldvals (map (eval-with env4) fieldexps))
  (env-set!* env4 fieldvars fieldvals)
  (eval body env4)
  env4
  )

;Equivalent of dot-calling in Grace: lets you look inside an object, and then call one of that object's methods.
(define (eval-send3 obj meth env)
  (let* ((env*     ((eval-with env) obj))
         )
    (eval meth env*)))


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
;(change encoding to use a char that can't be entered in Grace or to make hiddenvar inaccessible except by internal methods?)
;Makes a method with same names as var that returns value of hiddenva
;Makes a method with name ____:= that takes a value and sets hiddenvar to that value
(define (eval-initvar var val env)
  (let* ((hiddenvar (string->symbol (string-append "$" (symbol->string var))))
         (readmethod var)
         (modmethod (string->symbol (string-append (symbol->string var) ":=")))
         (env0 (env-extend* env (list hiddenvar) (list ((eval-with env) val))))
         (env1 (env-extend* env0 (list readmethod) (list (eval `(lambda () ,hiddenvar) env0))))
         (env2 (env-extend* env1 (list modmethod) (list (eval `(lambda (x) (set! ,hiddenvar x)) env1)))))
    ;(if (eq? val 'y) (displayln ((eval-with env) val)) (displayln "not y"))
    ;(display env2)
    ;(newline)
    ;Had bug where environment would not change for next step even after initializing variable in body right before.
    ;Solution was the following line.  Sets the original env to contain the just-added bindings
    (set-box! env (unbox env2))
    env2))

;Allows you to initialize a list of var objects.  Takes list of vars and list of initial values.
(define (eval-initvar* objs vals env)
  (match `(,objs ,vals)
    [`((,o . ,objs) (,v . ,vals))
     ;=>
     (eval-initvar* objs vals (eval-initvar o v env))]
    
        
    [`(() ())
     ; =>
     ;(display env)
     ;(newline)
     env]))
    

;This is what the var:= methods should be doing
;Probably wrong: rather than calling set on a var, we should be calling the var:= method in the first place to allow overriding
(define (env-Cset! env var value)
  (let* ((hidvar (string->symbol (string-append "$" (symbol->string var)))))
    ;(print env)
    (set-cell-value! (hash-ref (unbox env) hidvar) value)))

(define (env-Cset!* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     ; =>
     (begin
       (env-Cset! env v val)
       (env-Cset!* env vars values))]
    
    [`(() ())
     ; =>
     (void)]))
         
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

;like let, except it binds the values to their names as defs rather than vars
(define (eval-def bindings body env)
  (let* ((vars (map car bindings))
         (exps (map cadr bindings))
         (vals (map (eval-with env) exps))
         (env* (env-extdef* env vars vals)))
    (eval body env*)))

; applies a procedure to arguments:
(define (apply-proc f values)
  (match f
    [`(closure (lambda ,vs ,body) ,env) 
     ; =>
     ;change to def at some point
     (eval body (eval-initvar* vs values env))
     ]
    
    [`(primitive ,p)
     ; =>
     (apply p values)]))

;; Environments map variables to mutable cells 
;; containing values.  Also have list of objects.

(define-struct cell ([value #:mutable]))

; empty environment:
(define (env-empty) (hash))

; initial environment, with bindings for primitives:
(define (env-initial)
   (env-extend* 
    (env-extend* 
   (box (env-empty))
   ;first takes a whole list of primitives and binds them to racket equivalents
   ;many of these will need to be replaced: all the math ones will need to extract values out of new number objects
   ;and then call primitive version rather than being in current form
   '(+  -  /  *  %   <= >= eq? void  display  newline string-append cons list eval eval-with false? number->string null list? == 
        print)
   (map (lambda (s) (list 'primitive s))
   `(,+ ,- ,/ ,* ,modulo ,<= ,>= ,eq? ,void ,display ,newline ,string-append ,cons ,list ,eval ,eval-with ,false? ,number->string ,null ,list? ,equal? 
        ,(lambda (x) (match x 
                       ;if x is an object, check if it has an asString method defined, and call that
                       ;ideally, all objects will have asString defined
                       [(? box?) (if (hash-has-key? (unbox x) 'asString) (begin (display (eval `asString x)) (newline)) (begin (display x) (newline)))]
                       [any (begin (display x) (newline))])))))
    ;next, extends environment further to add tru and fals objects
    ;naming is intentional to avoid any confusion with primitive true and false but should be changeable without causing any issues.
    ;The only thing users should see is the asString method, so it may be fine as-is.
    '(tru fals 
          ;<
          )
    (let* ((env0 (env-extend* (box (env-empty)) '(asString) `("true"))) 
              (env1 (env-extend* (box (env-empty)) '(asString) `("false")))) 
                (begin (set-box! env0 (unbox (env-extend* env0 '(not prefix!) `((closure (lambda () ,env1) ,env1) (closure (lambda () ,env1) ,env1)))))
                       (set-box! env1 (unbox (env-extend* env1 '(not prefix!) `((closure (lambda () ,env0) ,env0) (closure (lambda () ,env0) ,env0)))))
                       `(,env0 ,env1))))
   )


; looks up a value:
(define (env-lookup env var)
  (if (primitive? var) ((lambda (s) (list 'primitive s)) var) 
  (match (hash-ref (unbox env) var)
    [(? cell?)  
     (cell-value (hash-ref (unbox env) var))]
    [x
     (hash-ref (unbox env) var)])))
    

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

;extends environment, but instead of mapping var to a mutable cell containing the value, just maps it directly to the value
;eventually, set! method will change, and then we should change this so it looks like initvar, but without the var:= method.
(define (env-extdef* env invarbls values)
  (match `(,invarbls ,values)
    [`((,i . ,invarbls) (,val . ,values))
     ; =>
     (set-box! env (box (env-extend* (hash-set (unbox env) i val) invarbls values)))]
    
    [`(() ())
     ; =>
     env]))

;extend an environment mapping a list of values to void.
;Doesn't entirely match grace behavior: racket will allow you to call something like print on void
;while grace will call an error if you try to call a method on a var that is not yet mapped to anything.
(define (env-extempty env varbls)
  (match `(,varbls)
    [`((,v . ,varbls))
     ; =>
     (set-box! env (box (env-extempty (hash-set (unbox env) v (make-cell (void))) varbls)))]
    
    [`(())
     ; =>
     env]))


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



;; Evaluation tests.

; define new syntax to make tests look prettier:
(define-syntax 
  test-eval 
  (syntax-rules (====)
    [(_ program ==== value)
     (let ((result (eval (quote program) (env-initial))))
       (when (not (equal? program value))
         (error "test failed!")))]))

(test-eval
  ((lambda (x) (+ 3 4)) 20)
  ====
  7)

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

(test-eval
  (let ((x 100))
    (begin
      (set! x 20)
      x))
  ====
  20)

(test-eval
  (let ((x 1000))
    (begin (let ((x 10))
             20)
           x))
  ====
  1000)


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

;Testing env-lookup
;(print (env-lookup (env-initial) '+))

; read in a program, and evaluate:
(eval-program (read-all))


 