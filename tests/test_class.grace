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

