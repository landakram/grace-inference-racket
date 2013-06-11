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
    b:=(_ : Number) -> Done
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

obj.foo(2, "2", true)

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

//string::117: typecheck: initializing var  of type type Object_119 = { b() -> Number b:=() -> Number...#f) #s(grace:identifier "Number" #f) #s(grace:number 2)) #s(grace:method #s(grace:identifier "f...