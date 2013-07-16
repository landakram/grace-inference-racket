## List of changes to parsers

- Name of operators in expression changed to symbols uniformly.
- NEWLINE tokens insert #s(struct:grace:newline) instead of #<void>.
- type-definitions now insert a #s(struct:grace:type-def) instead of creating
    the type in the parser.
- The insertion of method definition inside of type definitions into the ast
    are changed to #s(struct:grace:method-def).
