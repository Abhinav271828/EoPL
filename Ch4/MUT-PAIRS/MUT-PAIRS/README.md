# The EXP-REFS Language
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
        - `extendenvrec` (subroutine `extendenvrec1`)
        - `applyenv`
        - `initenv`
    - the store interface (data)
        - `emptystore`
        - `newref`
        - `deref`
        - `setref`
    - `valueofprog :: String -> ExpVal`
* `example.hs`

## Syntax
```
Expression := let ID = Expression in Expression
           := - ( Expression , Expression )
           := newpair ( Expression , Expression )
           := left ( Expression )
           := right ( Expression )
           := if Bool then Expression else Expression
           := λ ID -> Expression
           := letrec ID = λ ID -> Expression , ... in Expression
           := ID ( ID )
           := ID
           := set ID = Expression
           := setleft ID = Expression
           := setright ID = Expression
           := begin Expression ; ... end
           := Num

ID         := String

Bool       := iszero Expression

Ref        := Expression
```

## Examples
```hs
-- evaluates to 1
p = "let x = newpair ( 2 , 3 ) \
    \in - ( right ( x ) , left ( x ) )"
```
