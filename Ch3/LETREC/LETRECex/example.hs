import Parse
import Interpret

p :: String -- evaluates to 1
p = "letrec even = λ x -> if iszero x \
                         \then 1 \
                         \else odd ( - ( x , 1 ) ) , \
           \odd = λ x -> if iszero x \
                        \then 0 \
                        \else even ( - ( x , 1 ) ) \
     \in - ( even ( 8 ) , odd ( 6 ) )"

q :: String -- evaluates to [ 7 , 7 , 7 , 7 , 7 ]
q = "letrec rep = λ x -> cons x rep ( x ) , \
           \take = λ n , l -> if iszero n \
                             \then [] \
                             \else cons car l \
                                       \take ( - ( n , 1 ) , cdr l ) \
     \in take ( 5 , rep ( 7 ) )"

r :: String -- evaluates to 40320
r = "let add = λ x , y -> - ( x , - ( 0 , y ) ) \
     \in letrec mult = λ x , y -> if iszero y \
                                 \then 0 \
                                 \else add ( x , mult ( x , - ( y , 1 ) ) ) \
         \in letrec fact = λ n -> if iszero n \
                                 \then 1 \
                                 \else mult ( n , fact ( - ( n , 1 ) ) ) \
             \in fact ( 8 )"
