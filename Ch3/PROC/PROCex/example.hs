import Interpret
import Parse

p :: String -- evaluates to 0
p = "let x = 200 \
     \in let f = λ z -> - ( z , x ) \
         \in let x = 100 \
             \in let g = λ z -> - ( z , x ) \
                 \in - ( f ( 1 ) , g ( 1 ) )"

q :: String -- evaluates to 0
q = "let add = λ x , y -> - ( x , - ( 0 , y ) ) \
     \in let mult = λ x , y -> if iszero y \
                              \then 0 \
                              \else add ( x , mult ( x , - ( y , 1 ) ) ) \
         \in if iszero - ( mult ( 3 , 4 ) , * ( 3 , 4 ) ) \
            \then 0 \
            \else 1"

r :: String -- evaluates to 720
r = "let makemult = λ maker , x , y -> \
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
