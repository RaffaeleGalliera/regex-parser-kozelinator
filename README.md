L'idea sarebbe di fare un parser per espressioni regolari. Data quindi
una stringa che contiene un'espressione regolare, costruire un abstract
syntax tree (AST).
Nella sintassi Haskell, potrebbe essere un dato costruito cosi`:

data RegExp = Epsilon | Unione RegExp RegExp | Star RegExp | Concat
RegExp RegExp | Empty |Term Char

Come simboli dell'alfabeto terminale potremmo tenere tutte le lettere
minuscole e i numeri.

Quindi data una stringa "a*(b+d)*" dovreste costruire un AST

Concat (Star (Term 'a')) (Star (Unione (Term 'b') (Term 'd') )) )

Altrimenti potrei assegnarvi due tesine separate.
