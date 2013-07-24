#lang grace

// ***** BROKEN TESTS *****
var a : Number := "Hello"   // Fixed [ ]
def b : String = 2          // Fixed [ ]
def d = 2 + "Hello"         // Fixed [ ]
type Object1 = {}                
def c : Object1 = object {}
c.foo()                     // Fixed [ ]

object {
  def e : Number = "Hello"  // Fixed [ ]
}   






// ***** REAL TESTS *****

class Dog.new(name, size) {
  var name := name
  var size := size
  
  method changeName(newName) {
    name := newName
    return name
  }
  
  method bark() {
    print("ruff!")
  }
}

def dog = Dog.new("Doug", 10)

print(dog.size)
print(dog.name)

dog.changeName("Max")

dog.bark()

var rightDog := false

if (dog.name() == "Max") then {
  rightDog := true                               
  print("All is well")
} else {
  print("Then poor dog is confused")
}
  
if (rightDog.not) then {
  print("You should go find the right owner")
}       

// Subtyping

type S = {
  a() -> Number
}

type T = {
  a() -> Number
  a:=(_ : Number) -> Done
}

method foo(arg : S) -> String{
  print(S)
  return "Done!"       
}

def x : T = object {
  var a : Number := 2
}

foo(x)
