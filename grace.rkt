#lang grace

var a := 4
var b := 4

def t = "hello"

var c := object {
   var v := "foo"
}

a.e()

method foo { 
    a + 3 + 5 + b
}