# The NL Language
## Files
* `parse.hs`
    - the AST (expression) datatype definition
    - `scanparse :: String -> AST`
* `translation.hs`
    - the NLAST (nameless expression) datatype definition
    - the static environment interface (data)
        - `emptysenv`
        - `extendsenv`
        - `applysenv`
        - `initsenv`
    - `translationofprog :: AST -> NLAST`
* `interpret.hs`
    - the ExpVal (expressed values) datatype definition
    - the Proc (procedures) datatype definition
    - the environment interface (data)
        - `emptyenv`
        - `extendenv`
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
           := ID ( ID )
           := ID
           := Num

ID         := String

Bool       := iszero Expression
```

## Examples
```hs
(evaluates to -5)
p = "let x = 7 \
     \in let y = 2 \
         \in let y = let x = - ( x , 1 ) \
                     \in - ( x , y ) \
             \in - ( - ( x , 8 ) , y )"
```

```hs
q = "let f = λ n -> - ( n , - ( 0 , n ) ) \
     \in f ( f ( f ( 2 ) ) )"
```
