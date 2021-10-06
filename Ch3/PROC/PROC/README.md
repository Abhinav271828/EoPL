# The PROC Language
## Files
* `parse.hs`
    - the AST (expression) datatype definition
    - `scanparse :: String -> AST`{.hs}
* `interpret.hs`
    - the ExpVal (expressed values) datatype definition
    - the Proc (procedures) datatype definition (evaluated in defining environment)
    - the environment interface (procedural)
        - `emptyenv`{.hs}
        - `extendenv`{.hs}
        - `applyenv`{.hs}
        - `initenv`{.hs}
    - `valueofprog :: String -> ExpVal`{.hs}
* `example.hs`

## Syntax
```
Expression := let ID = Expression in Expression
           := - ( Expression , Expression )
           := * ( Expression , Expression )
           := if Bool then Expression else Expression
           := λ ID -> Expression
           := ID ( ID )
           := ID
           := Num

ID         := String

Bool       := iszero Expression
```


## Examples
```hs
-- evaluates to 4
p = "let x = 5 \
     \in let f = λ x -> * ( 2 , x ) \
         \in let y = f ( 3 ) \
             \in - ( f ( x ) , y )"
```

```hs
-- evaluates to 0
q = "let x = - ( 7 , 7 ) \
     \in if iszero x \
           \then let y = * ( 3 , x) \
                 \in y \
           \else let z = - ( 0 , x) \
                 \in * ( z , z)"
```
