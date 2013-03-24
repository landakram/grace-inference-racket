#lang grace
var a := "foo"
var c := 5
var b := 4 + 4 - 10 * 30 / 38 + c

def g = "this is a definition"

method foo(s : String) -> Number {
    return 4
}

4 + 5 + 4 + foo("hello") 

type A = {
    foo() -> Number
}

var d : A := object {
    method foo() -> Number {
        5
    }
}

var d := object {
    var a := 4
    var b := 7
    method bar(s : String) -> Number {
        self.a + self.b // doesn't get typechecked without type annotation
        self.a := 3
        self.a
        a := 5
        bar("hello")
        self.bar("hello")
        4
    }
    var c := self.bar("foo")
}
