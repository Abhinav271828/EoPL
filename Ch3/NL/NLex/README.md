# The PROC Language
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
        - `extendenvrec`
        - `applyenv`
        - `initenv`
    - `valueofprog :: String -> ExpVal`
* `example.hs`

## Syntax
```
Expression := let ID = Expression , ... in Expression
           := - ( Expression , Expression )
           := List
           := car List
           := cdr List
           := if Bool then Expression else Expression
           := cond ( Bool -> Expression , ... )
           := λ ID , ... -> Expression
           := letrec ID = λ ID , ... -> Expression , ... in Expression
           := ID ( ID , ... )
           := ID
           := Num

List       := cons ( Expression , List )
           := []

ID         := String

Bool       := iszero Expression
```

## Examples
```hs
-- evaluates to 1
p = "letrec even = λ x -> cond ( iszero x -> 1 , \
                                \iszero 0 -> odd ( - ( x , 1 ) ) ) , \
           \odd = λ x -> cond ( iszero x -> 0 , \
                               \iszero 0 -> even ( - ( x , 1 ) ) ) \
     \in - ( even ( 8 ) , odd ( 6 ) )"
```
