## Installation

To install grace as a *#lang* language:

    $ cd hopper
    $ raco link grace

If you are using racket on a Mac, you may need to add raco to your path.
In ~/.bash_profile add a line:

<pre>
    export PATH=/Applications/Racket\ <b>&lt;version&gt;</b>/bin:$PATH
</pre>
    
(replacing `<version>` with whatever version of Racket you have)

Then do:

    $ source ~/.bash_profile
    
## Language Features Implemented

<pre>
 [x] Primitives
 [x] Variables
 [x] Definitions
 [x] Objects
 [-] Methods
    - [ ] Mixfix notation
    - [x] Assignment overriding
 [-] Types
    - [ ] Subtyping
 [-] Standard Functions
    - [-] Print
        * [x] taking a string or number without surrounding parentheses
        * [ ] taking a boolean without surrounding parentheses
    - [ ] String Interpolation
 [-] Conditionals
    - [x] if
    - [ ] elseif
    - [x] else
    - [ ] match/case
 [-] Operators
 [ ] Loops
 [ ] Lists
 [x] Classes
    - [ ] Inheritance
 [ ] Blocks
 [ ] Modules
</pre>
