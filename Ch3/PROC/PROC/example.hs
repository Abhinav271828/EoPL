import Interpret

p :: String -- evaluates to Numv (-100)
p = "let x = 200 \
     \in let f = \\ z -> - ( z , x ) \
         \in let x = 100 \
             \in let g = \\ z -> - ( z , x ) \
                 \in - ( f ( 1 ) , g ( 1 ) )"
