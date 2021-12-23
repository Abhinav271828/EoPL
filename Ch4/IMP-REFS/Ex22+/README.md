# The EXP-REFS Language
## Files
* `parse.hs`
    - the AST (expression) datatype definition
    - the Stmt (statement) datatype definition
    - the Prog (program) datatype definition
    - `scanparse :: String -> AST`
* `interpret.hs`
    - the ExpVal (expressed values) datatype definition
    - the Proc (procedures) datatype definition
    - the Subr (subroutines) datatype definition
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
    - `run :: String -> IO ()`
* `example.hs`

## Syntax
```
Prog       := Stmt

Stmt       := ID = Expression
           := print Expression
           := { Stmt ; ... }
           := if Expression Stmt Stmt
           := while Expression Stmt
           := Var ID , ... ; Stmt
           := Expression ( Expression )

Expression := let ID = Expression in Expression
           := - ( Expression , Expression )
           := if Bool then Expression else Expression
           := List
           := car List
           := cdr List
           := λ ID -> Expression
           := μ ID -> Stmt
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
-- prints -1
p = "var x , y , f ; \
                     \{ x = 3 ; \
                       \y = 4 ; \
                       \f = μ t -> \
                               \print t ; \
                       \f ( - ( x , y ) ) }"
-- prints 12
q = "var f , x ; \
                 \{ f = λ x -> -> - ( x , 3 ) ; \
                   \x = 3 ; \
                   \print f ( 15 ) }"

```
