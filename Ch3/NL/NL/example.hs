import Interpret
import Parse
import Translation

p :: String -- evaluates to Numv (-100)
p = "let x = 200 \
     \in let f = λ z -> - ( z , x ) \
         \in let x = 100 \
             \in let g = λ z -> - ( z , x ) \
                 \in - ( f ( 1 ) , g ( 1 ) )"
