# The LETex Language
## Files
* `parse.hs`
    - the AST (expression) datatype definition
    - `scanparse :: String -> AST`{.hs}
* `environment.hs`
    - the ExpVal (expressed values) datatype definition
    - the environment interface (procedural)
        - `emptyenv`{.hs}
        - `extendenv`{.hs}
        - `applyenv`{.hs}
        - `initenv`{.hs}
* `interpret.hs`
    - `valueofprog :: String -> ExpVal`{.hs}
* `example.hs`

## Syntax
```
Expression := let ID = Expression in Expression
           := lets ID = Expression , ... in Expression
           := unpack ( ID , ... ) = List in Expression
           := - ( Expression , Expression )
           := + ( Expression , Expression )
           := List
           := car List
           := cdr List
           := if Bool then Expression else Expression
           := cond ( Bool -> Expression , ... )
           := ID
           := Num
           := - Num

List       := cons Expression List
           := []
           := list ( Expression , ... )

ID         := String

Bool       := iszero Expression
           := == Expression Expression
           := < Expression Expression
           := > Expression Expression
```

## Examples
```hs
-- (evaluates to -5)
p = "let x = 7 \
     \in let y = 2 \
         \in let y = let x = - ( x , 1 ) \
                     \in - ( x , y ) \
             \in - ( - ( x , 8 ) , y )"
```

```hs
-- (evaluates to 12)
q = "lets a = 5 , \
         \b = 3 , \
         \c = 4 \
      \in unpack ( x , y , z ) = list ( a , b , c ) \
          \in + ( x , + ( y , z) )"
```

```hs
-- (evaluates to 0)
r = "let l = cons - 1 cons 0 cons 1 [] \
     \in unpack ( p , q , r ) = l \
         \in cond ( iszero p -> p , \
                   \< q r -> q , \
                   \> 0 r -> r )"
```
