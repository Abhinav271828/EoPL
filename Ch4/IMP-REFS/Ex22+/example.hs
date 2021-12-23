import Parse
import Interpret

p :: String -- prints -1
p = "var x , y , f ; \
                     \{ x = 3 ; \
                       \y = 4 ; \
                       \f = μ t -> \
                               \print t ; \
                       \f ( - ( x , y ) ) }"

q :: String -- prints 12
q = "var f , x ; \
                 \{ f = λ x -> -> - ( x , 3 ) ; \
                   \x = 3 ; \
                   \print f ( 15 ) }"
