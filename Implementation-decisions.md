Any call to a variable before it is initialized gives an error.  For example:

    print val
    var val := 2
  
will give an error saying val has not yet beeon initialized.
The same is true inside an object.  For example:

    object{
    outer.val
    }
    var val := 2
  
will give the same error.
If the call to val is inside a method, the behavior depends on when the method is called.

    var x:= object{
      method foo() {
        outer.val
      }
    }
    var val := 2
    x.foo()
  
is ok, but calling foo before the initialization of val would give the error referred to above.

