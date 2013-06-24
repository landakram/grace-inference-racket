#lang grace

// TESTING CLASSES

class Cat.new(x) {
    def name : String = x
     
    method purr {
        print("Purr")
    }
     
    method mew {
        print("Meow")
    }
}

var c := Cat.new("Kitty")

c.purr()
c.mew()


// TESTING TYPES

type X = {
    foo(_ : String) -> String
}

var x : X := object {
    method foo(s : String) -> String {
        s
    }
}

def y : String = x.foo("Hello")

print(y)


// TESTING OBJECTS

var y := object {
    var val := 1
        
    method foo {
        print(self.val)
        self.val := self.val + 1
    }
}

y.foo
