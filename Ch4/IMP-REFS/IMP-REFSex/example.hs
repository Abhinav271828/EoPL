import Parse
import Interpret

p :: String -- evaluates to 3
p = "letmutable x = 17 \
    \in letmutable p = λ y -> - ( y , x ) \
       \in - ( setdynamic x = 17 during p ( 22 ) , 
               p ( 13 ) )"

q :: String -- evaluates to 1
q = "letrec 0mod3 = λ x -> if iszero x \
                          \then 1 \
                          \else 2mod2 ( - ( x , 1 ) ) , \
           \1mod3 = λ x -> if iszero x \
                          \then 0 \
                          \else 0mod2 ( - ( x , 1 ) ) \
           \2mod3 = λ x -> if iszero x \
                          \then 0 \
                          \else 1mod2 ( - ( x , 1 ) ) \
    \in 0mod3 ( 9 )"

