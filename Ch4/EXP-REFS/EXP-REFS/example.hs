import Parse
import Interpret

p :: String -- evaluates to 1
p = "let g = let c = newref ( 10 ) \
            \in λ * -> begin \
                        \setref ( c , -  ( deref ( c ) , 1 ) ) ; \
                        \deref ( c ) \
                      \end \
    \in let a = g ( 11 ) \
       \in let b = g ( 11 ) \
          \in - ( a , b )"

q :: String -- evaluates to 11
q = "let x = newref ( newref ( 0 ) ) \
    \in begin \
         \setref ( deref ( x ) , 11 ) ; \
         \deref ( deref ( x ) ) \
       \end"
