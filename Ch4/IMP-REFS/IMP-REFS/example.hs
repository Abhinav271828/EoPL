import Parse
import Interpret

p :: String -- evaluates to 1
p = "let g = let c = 10 \
            \in λ * -> begin \
                        \set c = - ( c , 1 ) ; \
                        \c \
                      \end \
    \in let a = g ( 11 ) \
       \in let b = g ( 11 ) \
          \in - ( a , b )"
