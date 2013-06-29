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
var x := object {
    def c = "Hello"
}
print(x.c)

//Test 16 : OK
var x := object {
    method foo {
        print("Hello")
    }
}
x.foo
x := object {
    method bar {
        "World"
    }
}
print(x.bar)


//Test 17: OK
var x := object {
    var val := 1
    //val := 2
}
print(x.val)
x.val := 2
print(x.val)

//Test 18: OK
var x := object {
    var val := 1
    method foo {
        print(self.val)
        self.val := self.val + 1
    }
}
x.foo
x.foo
x.foo

//Test 19: OK
print(1 == 1)
print(1 == 2)
print (2 == (1 + 1))
print(false == true)
print(false == false)
print("Hello" == "world")
print("Hello" == "Hello")
var x := object {
    var v := 1
}
var y := object {
    var v := 1
}
print(x == y)
print(x == x)

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
var x := object {
    var v := 1
}
var y := object {
    var v := 1
}
print(x != y)
print(x != x)






