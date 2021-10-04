import Interpret

prog = "letrec double = λ x -> if iszero x \
                               \then 0 \
                               \else - ( double ( - ( x , 1 ) ) , - ( 0 , 2 ) ) \
        \in double ( 6 )"
