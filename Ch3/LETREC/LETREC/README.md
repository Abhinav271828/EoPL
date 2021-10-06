# The LETREC Language
## Files
* `parse.hs`
    - the AST (expression) datatype definition
    - `scanparse :: String -> AST`
* `interpret.hs`
    - the ExpVal (expressed values) datatype definition
    - the Proc (procedures) datatype definition
    - the environment interface (procedural)
        - `emptyenv`
        - `extendenv`
        - `extendenvrec`
        - `applyenv`
        - `initenv`
    - `valueofprog :: String -> ExpVal`
* `example.hs`

## Syntax
```
Expression := let ID = Expression in Expression
           := - ( Expression , Expression )
           := if Bool then Expression else Expression
           := λ ID -> Expression
           := letrec ID = λ ID -> Expression in Expression
           := ID ( ID )
           := ID
           := Num

ID         := String

Bool       := iszero Expression
```

## Examples
```hs
-- evaluates to 1
p = "letrec even = λ x -> if iszero x \
                         \then 1 \
                         \else if iszero - ( x , 1 ) \
                              \then 0 \
                              \else even ( - ( x , 2 ) ) \
    \in even ( 8 )"
```
