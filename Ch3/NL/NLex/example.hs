import Interpret
import Parse
import Translation

p :: String -- evaluates to 1
p = "letrec even = λ x -> cond ( iszero x -> 1 , \
                                \iszero 0 -> odd ( - ( x , 1 ) ) ) , \
           \odd = λ x -> cond ( iszero x -> 0 , \
                               \iszero 0 -> even ( - ( x , 1 ) ) ) \
     \in - ( even ( 8 ) , odd ( 6 ) )"

