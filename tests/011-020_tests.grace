#lang grace

//Test 11 : FIXME (.. not in, lists not in)
//var x := 0..5
//print("OK")


////Test 12 : FIXME (.. not in, lists not in, for not in)
//for (1..5) do { v ->
//    print(v)
//}

//Test 13 : OK
method foo {
    print("OK 1")
}
method bar(x) {
    print("OK " ++ x)
}
foo
bar(2)
bar(3)

//Test 14: OK
var x2 := object {
    var v := 1
}
print(x2.v)

//Test 15: OK
var x3 := object {
    def c = "Hello"
}
print(x3.c)

//Test 16 : OK
var x4 := object {
    method foo {
        print("Hello")
    }
}
x4.foo
var x4sv := object {
    method bar {
        "World"
    }
}
print(x4sv.bar)


//Test 17: OK
var x5 := object {
    var val := 1
    //val := 2
}
print(x5.val)
x5.val := 2
print(x5.val)

//Test 18: OK
var x6 := object {
    var val := 1
    method foo {
        print(self.val)
        self.val := self.val + 1
    }
}
x6.foo
x6.foo
x6.foo

//Test 19: OK
print(1 == 1)
print(1 == 2)
print (2 == (1 + 1))
print(true == false)
print(true == true)
print(false == true)
print(false == false)
print("Hello" == "world")
print("Hello" == "Hello")
var x7 := object {
    var v := 1
}
var y1 := object {
    var v := 1
}
print(x7 == y1)
print(x7 == x7)

//Test 20: OK
print(1 != 1)
print(1 != 2)
print (2 != (1 + 1))
print(true != false)
print(true != true)
print(false != true)
print(false != false)
print("Hello" != "world")
print("Hello" != "Hello")
var x8 := object {
    var v := 1
}
var y2 := object {
    var v := 1
}
print(x8 != y2)
print(x8 != x8)





