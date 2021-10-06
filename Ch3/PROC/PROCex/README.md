# The PROCex Language
## Files
* `parse.hs`
    - the AST (expression) datatype definition
    - `scanparse :: String -> AST`
* `interpret.hs`
    - the ExpVal (expressed values) datatype definition
    - the Proc (procedures) datatype definition (evaluated in calling environment)
    - the environment interface (procedural)
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
           := * ( Expression , Expression )
           := if Bool then Expression else Expression
           := λ ID , ... -> Expression
           := ID ( ID , ... )
           := ID
           := Num

ID         := String

Bool       := iszero Expression
```

## Examples
```hs
-- evaluates to 0
p = "let x = 200 \
     \in let f = λ z -> - ( z , x ) \
         \in let x = 100 \
             \in let g = λ z -> - ( z , x ) \
                 \in - ( f ( 1 ) , g ( 1 ) )"
```

```hs
-- evaluates to 0
q = "let add = λ x , y -> - ( x , - ( 0 , y ) ) \
     \in let mult = λ x , y -> if iszero y \
                              \then 0 \
                              \else add ( x , mult ( x , - ( y , 1 ) ) ) \
         \in if iszero - ( mult ( 3 , 4 ) , * ( 3 , 4 ) ) \
            \then 0 \
            \else 1"
```

```hs
-- evaluates to 720
f = "let makemult = λ maker , x , y -> \
                        \if iszero x \
                        \then 0 \
                        \else - ( maker ( maker , - ( x , 1 ) , y ) , - ( 0 , y ) ) \
     \in let times = λ x , y -> \
                         \makemult ( makemult , x , y ) \
         \in let makefact = λ maker , n -> \
                                \if iszero n \
                                \then 1 \
                                \else times ( maker ( maker , - ( n , 1 ) ) , n ) \
             \in let fact = λ n -> \
                                \makefact ( makefact , n ) \
                 \in fact ( 6 )"
```
