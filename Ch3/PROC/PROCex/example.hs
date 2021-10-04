import Interpret

prog :: String
prog = "let x = 200 \
        \in let f = λ z -> - ( z , x ) \
            \in let x = 100 \
                \in let g = λ z -> - ( z , x ) \
                    \in - ( f ( 1 ) , g ( 1 ) )"

p = "let makemult = λ maker , x -> if iszero x \
                                    \then 0 \
                                    \else - ( maker ( maker , - ( x , 1 ) ) , 4 ) \
         \in let times4 = λ x -> makemult ( makemult , x ) \
             \in times4 ( 3 )"

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


t = "let a = 3 \
     \in let p = λ x -> - ( x , a ) \
         \in let a = 5 \
             \in - ( a , p ( 2 ) )"

pari = "let even = λ n -> \
                       \if iszero n \
                       \then 1 \
                       \else odd ( - ( n , 1 ) ) \
            \in let odd = λ n -> \
                              \if iszero n \
                              \then 0 \
                              \else even ( - ( n , 1 ) ) \
                \in odd ( 13 )"
