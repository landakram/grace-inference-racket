#lang grace

// TESTING CLASSES

class Cat {
  def name : String = "kitty"
   
  method purr {
    print("Purr")
  }
   
  method mew {
    print("Meow")
  }
}

var c := Cat.new()

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

var x := object {
  var val := 1
      
  method foo {
    print(self.val)
    self.val := self.val + 1
  }
}

x.foo
