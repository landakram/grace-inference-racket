#lang grace


type Object_3 = {
    a() -> Number
    a:=() -> Number
    bar() -> Number
    c() -> Number
    c:=() -> Number
}

var d : Object_3 := object {
    var a : Number := 4
    method bar()-> Number {
        4
    }
    var c : Number := self.bar
    self.c:= 3
}

var y := 3
var z := y + 4



