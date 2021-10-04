import Interpret

prog = "letrec double = λ x -> if iszero x \
                               \then 0 \
                               \else - ( double ( - ( x , 1 ) ) , - ( 0 , 2 ) ) \
        \in double ( 6 )"

prod = "letrec mult = λ x , y -> if iszero y \
                                 \then 0 \
                                 \else - ( mult ( x , - ( y , 1 ) ) , - ( 0 , x ) ) \
        \in mult ( 13 , 17 )"

pari = "letrec even = λ x -> if iszero x \
                             \then 1 \
                             \else odd ( - ( x , 1 ) ) , \
              \odd = λ x -> if iszero x \
                            \then 0 \
                            \else even ( - ( x , 1 ) ) \
        \in odd ( 13 )"

rep = "letrec rep = λ x -> cons x rep ( x ) , \
             \take = λ n , l -> if iszero n \
                                \then [] \
                                \else cons car l take ( - ( n , 1 ) , l ) \
       \in take ( 5 , rep ( 7 ) )"
