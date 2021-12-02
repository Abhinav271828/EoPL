# The EXP-REFSex Language
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
           := if Bool then Expression else Expression
           := List
           := car List
           := cdr List
           := λ ID , ... -> Expression
           := letrec ID = λ ID , ... -> Expression , ... in Expression
           := ID ( ID , ... )
           := ID
           := newref ( Expression )
           := deref ( Ref )
           := setref ( Ref , Expression )
           := begin Expression ; ... end
           := Num

List       := cons Expression Expression
           := []

ID         := String

Bool       := iszero Expression

Ref        := Expression
```

## Examples
```hs
-- evaluates to 1
p = "let g = let c = newref ( 10 ) \
            \in λ * -> begin \
                        \setref ( c , -  ( deref ( c ) , 1 ) ) ; \
                        \deref ( c ) \
                      \end \
    \in let a = g ( 11 ) \
       \in let b = g ( 11 ) \
          \in - ( a , b )"

-- evaluates to 11
q = "let x = newref ( newref ( 0 ) ) \
    \in begin \
         \setref ( deref ( x ) , 11 ) ; \
         \deref ( deref ( x ) ) \
       \end"
```
