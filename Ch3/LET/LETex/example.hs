import Parse
import Environment
import Interpret

p :: String -- evaluates to -5
p = "let x = 7 \
     \in let y = 2 \
         \in let y = let x = - ( x , 1 ) \
                     \in - ( x , y ) \
             \in - ( - ( x , 8 ) , y )"

q :: String -- evaluates to 12
q = "lets a = 5 , \
         \b = 3 , \
         \c = 4 \
      \in unpack ( x , y , z ) = list ( a , b , c ) \
          \in + ( x , + ( y , z) )"

r :: String -- evaluates to 0
r = "let l = cons - 1 cons 0 cons 1 [] \
     \in unpack ( p , q , r ) = l \
         \in cond ( iszero p -> p , \
                   \< q r -> q , \
                   \> 0 r -> r )"
