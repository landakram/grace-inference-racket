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
var x := object {
    var v := 1
}
print(x.v)

//Test 15: OK
var x2 := object {
    def c = "Hello"
}
print(x2.c)

//Test 16 : OK
var x3 := object {
    method foo {
        print("Hello")
    }
}
x3.foo
x3 := object {
    method bar {
        "World"
    }
}
print(x3.bar)


//Test 17: OK
var x4 := object {
    var val := 1
    //val := 2
}
print(x4.val)
x4.val := 2
print(x4.val)

//Test 18: OK
var x5 := object {
    var val := 1
    method foo {
        print(self.val)
        self.val := self.val + 1
    }
}
x5.foo
x5.foo
x5.foo

//Test 19: OK
print(1 == 1)
print(1 == 2)
print (2 == (1 + 1))
print(false == true)
print(false == false)
print("Hello" == "world")
print("Hello" == "Hello")
var x6 := object {
    var v := 1
}
var y := object {
    var v := 1
}
print(x == y)
print(x6 == x6)

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
var x7 := object {
    var v := 1
}
var y2 := object {
    var v := 1
}
print(x7 != y2)
print(x7 != x7)






