#lang grace

// ***** BROKEN TESTS *****
// var a : Number := "Hello"   // Fixed [x] Invalid assignment.
// var a : Num                 // Fixed [x] Invalid type annotation.
// def b : String = 2          // Fixed [x] Invalid assignment.
// def b : Stung = 5           // Fixed [x] Invalid type annotation.
// def d = 2 + "Hello"         // Fixed [x] Invalid operation.
//2 - 3
type Object1 = {}                
def c : Object1 = object {}
// c.foo()                     // Fixed [x] No such method.

// object {
//   def e : Number = "Hello"  // Fixed [x] Invalid assignment.
//   
//   method foo() -> String {
//     "bar"
//   }
// }

// type Object2 = {
//   foo(x : Number) -> Stringify // Fixed [x] Undefined type
// }

// type Object3 = {
//   b() -> String
// }
// def a : Object3 = object {}    // Fixed [x] Subtyping

// var x : Number := 2            // Fixed [x] Re-declaring variables.
// x := "Hello"                   // Fixed [x] Wrong assignment.



// ***** REAL TESTS *****

class Dog.new(name, size) {
  var name := name
  var size : Number := size
  
  method changeName(newName) {
    name := newName
    return name
  }
  
  method bark() {
    print("ruff!")
  }
}

object {
  def name = "Hello"
   
  method sayName() {
    print(self.name)
  }
   
  self.sayName()
   
  def c = object {
    def d = 4
  }     
}   

type DogType = {
  bark() -> Done
  changeName(newName : String) -> String
  name() -> String
  name:=(_ : String) -> Done
  size() -> Number
  size:=(_ : Number) -> Done
}  

def dog : DogType = Dog.new("Doug", 10)

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

method foo(arg : S) -> String {
  print(arg)
  return "Done!"
  self.x()
}

def x : T = object {
  var a : Number := 2
}

foo(x)

//object {
//  object {    
//    outer.outer.foo(x)
//  }
//}

