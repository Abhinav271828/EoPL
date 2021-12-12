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
           := if Bool then Expression else Expression
           := List
           := car List
           := cdr List
           := λ ID -> Expression
           := letrec ID = λ ID -> Expression , ... in Expression
           := ID ( ID )
           := ID
           := set ID = Expression
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
p :: String -- evaluates to 0
p = "letrec 0mod2 = λ x -> if iszero x \
                          \then 1 \
                          \else 1mod2 ( - ( x , 1 ) ) , \
           \1mod2 = λ x -> if iszero x \
                          \then 0 \
                          \else 0mod2 ( - ( x , 1 ) ) \
    \in 0mod2 ( 5 )"

q :: String -- evaluates to 1
q = "letrec 0mod3 = λ x -> if iszero x \
                          \then 1 \
                          \else 2mod2 ( - ( x , 1 ) ) , \
           \1mod3 = λ x -> if iszero x \
                          \then 0 \
                          \else 0mod2 ( - ( x , 1 ) ) \
           \2mod3 = λ x -> if iszero x \
                          \then 0 \
                          \else 1mod2 ( - ( x , 1 ) ) \
    \in 0mod3 ( 9 )"
```
