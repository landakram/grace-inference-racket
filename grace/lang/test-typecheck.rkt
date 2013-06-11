#lang racket

(require "typecheck.rkt"
         "parse.rkt")

(define (p in)
  (parse (object-name in) in))

(define a (p (open-input-string "
var b := object {
    var a := \"Hello\"

    method foo() -> Number {
        print(\"Hello\")
        4
    }
}

print(b.foo())
")))

(display
  (typecheck a))
