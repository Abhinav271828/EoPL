# The LET Language
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
           := - ( Expression , Expression )
           := if Bool then Expression else Expression
           := ID
           := Num

ID         := String

Bool       := iszero Expression
```

## Examples
```hs
-- evaluates to -5
p = "let x = 7 \
     \in let y = 2 \
         \in let y = let x = - ( x , 1 ) \
                     \in - ( x , y ) \
             \in - ( - ( x , 8 ) , y )"
```
