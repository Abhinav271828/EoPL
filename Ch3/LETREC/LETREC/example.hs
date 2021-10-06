import Parse
import Interpret

p :: String -- evaluates to 1
p = "letrec even = λ x -> if iszero x \
                         \then 1 \
                         \else if iszero - ( x , 1 ) \
                              \then 0 \
                              \else even ( - ( x , 2 ) ) \
    \in even ( 8 )"
