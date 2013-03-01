#lang grace
var a : String := "foo"
var c : Number := 3
var b : Number := 4 + 4 - 10 * 30 / 38 + c

c := 45 + c

method foo(s : String) -> Number {
    return 4
}

4 + 5 + 4 + foo("hello")

var d : Dynamic := object {
    var a : Number := 4
    var b : Number := 7
    method bar(s : String) -> Number {
	    	self.a + self.b
		self.a := 5
		self.a	
		a := 5
		bar("hello")
		self.bar("hello")
		4
    }
    
    var c : Number := self.bar("foo")
}

d.b