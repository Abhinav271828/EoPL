# The LETRECex Language
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
           := List
           := car List
           := cdr List
           := λ ID , ... -> Expression
           := letrec ID = λ ID , ... -> Expression , ... in Expression
           := ID ( ID , ... )
           := ID
           := Num

List       := cons Expression Expression
           := []

ID         := String

Bool       := iszero Expression
```

## Examples
```hs
-- evaluates to 1
p = "letrec even = λ x -> if iszero x \
                         \then 1 \
                         \else odd ( - ( x , 1 ) ) , \
           \odd = λ x -> if iszero x \
                        \then 0 \
                        \else even ( - ( x , 1 ) ) \
     \in - ( even ( 8 ) , odd ( 6 ) )"
```

```hs
-- evaluates to [ 7 , 7 , 7 , 7 , 7 ]
q = "letrec rep = λ x -> cons x rep ( x ) , \
           \take = λ n , l -> if iszero n \
                             \then [] \
                             \else cons car l \
                                       \take ( - ( n , 1 ) , cdr l ) \
     \in take ( 5 , rep ( 7 ) )"
```

```hs
-- evaluates to 40320
r = "let add = λ x , y -> - ( x , - ( 0 , y ) ) \
     \in letrec mult = λ x , y -> if iszero y \
                                 \then 0 \
                                 \else add ( x , mult ( x , - ( y , 1 ) ) ) \
         \in letrec fact = λ n -> if iszero n \
                                 \then 1 \
                                 \else mult ( n , fact ( - ( n , 1 ) ) ) \
             \in fact ( 8 )"
```
