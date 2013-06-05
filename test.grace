#lang grace

type Object_3 = {
    a() -> Number
    a:=() -> Number
    bar() -> Number
    c() -> Number
    c:=() -> Number
}

var obj := object {
	method foo() -> String {
            print("World")
		return "Hello"
	}
}

obj.foo()

self.print("hello")

method foo() -> String {
	"Hello"
}

//var d := object {
//    var a : Number := 4
//    method bar()-> Number {
//        4
//    }
//    var c : Number := self.bar
//    self.c:= 3
//}

//var h:= d
//var z := d.c

//d+4
