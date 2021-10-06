import Parse
import Interpret

p :: String -- evaluates to 4
p = "let x = 5 \
     \in let f = λ x -> * ( 2 , x ) \
         \in let y = f ( 3 ) \
             \in - ( f ( x ) , y )"

q :: String -- evaluates to 0
q = "let x = - ( 7 , 7 ) \
     \in if iszero x \
           \then let y = * ( 3 , x) \
                 \in y \
           \else let z = - ( 0 , x) \
                 \in * ( z , z)"
