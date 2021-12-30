import Parse
import Interpret

p :: String -- evaluates to 1
p = "let x = newpair ( 2 , 3 ) \
    \in - ( right ( x ) , left ( x ) )"
