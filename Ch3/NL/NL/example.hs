import Interpret
import Parse
import Translation

p :: String -- evaluates to -5
p = "let x = 7 \
     \in let y = 2 \
         \in let y = let x = - ( x , 1 ) \
                     \in - ( x , y ) \
             \in - ( - ( x , 8 ) , y )"

q :: String -- evaluates to 16
q = "let f = λ n -> - ( n , - ( 0 , n ) ) \
     \in f ( f ( f ( 2 ) ) )"
