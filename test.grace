#lang grace

type Object_3 = {
    a() -> Number
    a:=() -> Number
    bar() -> Number
    c() -> Number
    c:=() -> Number
}

type Object_119 = {
    b() -> Number
    b:=() -> Number
    foo(_ : Number, _ : String, _ : Boolean) -> String
}

var obj : Object_119 := object {
    var b : Number := 2
	method foo(x : Number, y : String, z : Boolean) -> String {
	        var w : Boolean := z
            print("World")
		return "Hello"
	}
}

obj.foo(2, "2", true) // TODO FIGURE OUT IN TYPECHECKER WHY APPEND IS NOT GETTING A LIST (LOOK AT BODY-STMT-TO-...)

self.print("hello")

method foo() -> String {
	"Hello"
}
method bar(z : Number) {
	foo()
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
