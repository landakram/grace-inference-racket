#lang grace

type Object_3 = {
    a() -> Number
    a:=() -> Number
    bar() -> Number
    c() -> Number
    c:=() -> Number
}

var obj := object {
    var b : Number := 2
	method foo(x : Number, y : String, z : Boolean) -> String {
	        var w : Boolean := z
            print("World")
		return "Hello"
	}
}

if (2 == 2) then { 
                2 
                 }

obj.foo(2, "2", true) // TODO FIGURE OUT IN TYPECHECKER WHY APPEND IS NOT GETTING A LIST (LOOK AT BODY-STMT-TO-...)

self.print("hello")

method foo() -> String {
	"Hello"
}
method bar(z : Number) {
	foo()
}

var a : Dynamic := 2

class foo { 
	var b := 2
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