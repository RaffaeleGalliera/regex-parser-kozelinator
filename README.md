## Regular Expressions Parser 

Print the AST (Abstract Syntax Tree) of Regular Expressions taken in input from a file with every RegExp separed by new lines.

RegExp should be a data structured like:
```haskell
data RegExp
    = Epsilon
    | Term Char
    | Star RegExp
    | Concatena RegExp RegExp
    | Unione RegExp RegExp
```

As terminal symbols we kept all the lower case letters and digits.

Given a string "a*(b+d)*" the program should build an AST like

`Concat (Star (Term 'a')) (Star (Union (Term 'b') (Term 'd')))`

Compile and try it with the example file "regex.txt"