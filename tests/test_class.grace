#lang grace

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

if (dog.name() == "Max") then {
  print("All is well")
} else {
  print("Your poor dog is confused")
}

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
  