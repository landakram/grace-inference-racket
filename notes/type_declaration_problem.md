## The problem

When trying to implement subtyping, checking subtyping of methods runs into issues because the methods have different forms for their signatures and return types. This examples does not necessarily demosntrate subtyping, but shows the difference in formats of the signatures and return types.

## Notes

- There are issues with the grace:object being formed in the parser for type-declarations.
- One way to solve would be have a grace:struct for type declarations then to convert it into a type object in the typechecker like with other declarations.

## The program

    type S = {
      a() -> Number
    }

    type T = {
      a() -> Number
      a:=(_ : Number) -> Done
    }

    method foo(arg : S) -> String {
      print(S)
      return \"Done!\"
    }

    def x : T = object {
      var a := 2
    }

    foo(x)
    

## Type implicitly stored by typechecker.

<pre>
subtype =>
    #(struct:object:grace:type:object%
        (
            #(struct:object:grace:type:method% a () #(struct:object:grace:type:dynamic*% () (#(struct:object:grace:type:method% print (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "Done" #f)) #(struct:object:grace:type:method% concat (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "String" #f)))))

            #(struct:object:grace:type:method% a:= (#s(grace:identifier "_" #(struct:object:grace:type:dynamic*% () (#(struct:object:grace:type:method% print (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "Done" #f)) #(struct:object:grace:type:method% concat (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "String" #f)))))) #(struct:object:grace:type:done% () (#(struct:object:grace:type:method% print (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "Done" #f)) #(struct:object:grace:type:method% concat (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "String" #f)))))
        )

        (
            #(struct:object:grace:type:method% print (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "Done" #f))

            #(struct:object:grace:type:method% concat (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "String" #f))
        )

        "Object_157"

        #\<procedure:parameter-procedure\>
    )
</pre>


## Type explicitly declared in program.

<pre>
supertype =>
    #(struct:object:grace:type:object%
        (
            #(struct:object:grace:type:method% a () #s(grace:identifier "Number" #f))

            #(struct:object:grace:type:method% a:= (#\<syntax::65 #s(grace:identifier "_" #s(gr...\>) #s(grace:identifier "Done" #f))))
        )

        (
            #(struct:object:grace:type:method% print (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "Done" #f))

            #(struct:object:grace:type:method% concat (#s(grace:identifier "other" #s(grace:identifier "Top" #f))) #s(grace:identifier "String" #f))
        )

        "T"

        #f
    )
</pre>
