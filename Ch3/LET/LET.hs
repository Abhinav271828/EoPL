import Data.Char

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Null deriving Show

getnum :: ExpVal -> Int
getnum (Numv x) = x
getbool :: ExpVal -> Bool
getbool (Boolv x) = x
--                           --

-- Environment Datatype --
type Env = String -> ExpVal

emptyenv :: Env
emptyenv = \_ -> Null

extendenv :: String -> ExpVal -> Env -> Env
extendenv var val e = \s -> if (s == var) then val else (applyenv e s)

applyenv :: Env -> String -> ExpVal
applyenv = ($)

initenv :: Env
initenv = emptyenv
--                      --

-- Expression Datatype --
data AST = Const Int          |
           Var String         |
           Diff AST AST       |
           Iszero AST         |
           Let String AST AST |
           Ifte AST AST AST
           deriving (Show, Eq)

valueofprog :: String -> ExpVal
valueofprog p = valueof (scanparse p) initenv

valueof :: AST -> Env -> ExpVal
valueof (Const  x      ) r = Numv x
valueof (Var    s      ) r = applyenv r s
valueof (Diff   e1  e2 ) r = let Numv v1 = valueof e1 r
                                 Numv v2 = valueof e2 r
                             in Numv (v1 - v2)
valueof (Iszero e      ) r = let Numv v = valueof e r
                             in Boolv (v == 0)
valueof (Let    var e b) r = let v = valueof e r
                             in valueof b (extendenv var v r)
valueof (Ifte   c   t e) r = let Boolv v = valueof c r
                             in if v then (valueof t r)
                                else (valueof e r)
--                     --

scanparse :: String -> AST
scanparse prog = let (p, []) = parse (words prog)
                  in p

parse :: [String] -> (AST, [String])
parse lex = case lex of
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
                                           (body, rest) = parse rem
              ("-":"(":ls) -> (Diff op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("iszero":ls) -> (Iszero body, rest)
                                  where (body, rest) = parse ls
              ("if":ls) -> (Ifte c t e, rest)
                              where (c, "then":rem) = parse ls
                                    (t, "else":erem) = parse rem
                                    (e, rest) = parse erem
              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else (Var l, ls)

p :: String -- evaluates to Numv (-5)
p = "let x = 7 \
     \in let y = 2 \
         \in let y = let x = - ( x , 1 ) \
                     \in - ( x , y ) \
             \in - ( - ( x , 8 ) , y )"
