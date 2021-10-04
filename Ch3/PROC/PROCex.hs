import Data.Char

-- Procedure Datatype (Prelim) --
type Proc = [ExpVal] -> Env -> ExpVal

proc :: [String] -> AST -> Proc
proc vs b = \es r -> valueof b
                        (foldr (\(v,e) env ->
                                  extendenv v e env)
                               r
                               (zip vs es))

applyproc :: Proc -> [ExpVal] -> Env -> ExpVal
applyproc = ($)
--                             --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Null

instance Show ExpVal where
  show (Numv  n) = "Numv " ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p [Numv 1] emptyenv)

getnum :: ExpVal -> Int
getnum (Numv x) = x
getbool :: ExpVal -> Bool
getbool (Boolv x) = x
getproc :: ExpVal -> Proc
getproc (Procv x) = x
--                           --

-- Environment Datatype --
type Env = String -> ExpVal

emptyenv :: Env
emptyenv = \_ -> Null

extendenv :: String -> ExpVal -> Env -> Env
extendenv var val e = \s -> if (s == var) then val else (e s)

applyenv :: Env -> String -> ExpVal
applyenv = ($)

initenv :: Env
initenv = emptyenv
--                      --

-- Expression Datatype --
data AST = Const Int          |
           Var String         |
           Diff AST AST       |
           Mult AST AST       |
           Iszero AST         |
           Let String AST AST |
           Ifte AST AST AST   |
           ProcE [String] AST |
           CallE AST [AST]
           deriving Show

valueofprog :: String -> ExpVal
valueofprog p = valueof (scanparse p) initenv

valueof :: AST -> Env -> ExpVal
valueof (Const  x         ) r = Numv x
valueof (Var    s         ) r = applyenv r s
valueof (Diff   e1  e2    ) r = let Numv v1 = valueof e1 r
                                    Numv v2 = valueof e2 r
                                in Numv (v1 - v2)
valueof (Mult   e1  e2    ) r = let Numv v1 = valueof e1 r
                                    Numv v2 = valueof e2 r
                                in Numv (v1 * v2)
valueof (Iszero e         ) r = let Numv v = valueof e r
                                in Boolv (v == 0)
valueof (Let    var e   b ) r = let v = valueof e r
                                in valueof b (extendenv var v r)
valueof (Ifte   c   t   e ) r = let Boolv v = valueof c r
                                in if v then (valueof t r)
                                   else (valueof e r)
valueof (ProcE  vs  b     ) r = Procv (proc vs b)
valueof (CallE  rat ran   ) r = let Procv fun = valueof rat r
                                    arg = [valueof a r | a <- ran]
                                  in applyproc fun arg r
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
              ("*":"(":ls) -> (Mult op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("iszero":ls) -> (Iszero body, rest)
                                  where (body, rest) = parse ls
              ("if":ls) -> (Ifte c t e, rest)
                              where (c, "then":rem) = parse ls
                                    (t, "else":erem) = parse rem
                                    (e, rest) = parse erem
              ("λ":ls) -> (ProcE args body, rest)
                                     where (args, "->":rem) = parseargs ls
                                           (body, rest) = parse rem
                                           parseargs as = (a:r, s)
                                                            where (a:x) = as
                                                                  (r,s) = case x of
                                                                        ",":rs -> parseargs rs
                                                                        "->":_ -> ([], x)
              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else case ls of
                                 "(":r -> (CallE (Var l) (args), rest)
                                            where (args, ")":rest) = parseargs r
                                                  parseargs as = (a:r, s)
                                                                    where (a,x) = parse as
                                                                          (r,s) = case x of
                                                                                   ",":rs -> parseargs rs
                                                                                   ")":_ -> ([],x)
                                 _ -> (Var l, ls)

prog :: String
prog = "let x = 200 \
        \in let f = λ z -> - ( z , x ) \
            \in let x = 100 \
                \in let g = λ z -> - ( z , x ) \
                    \in - ( f ( 1 ) , g ( 1 ) )"

p = "let makemult = λ maker , x -> if iszero x \
                                    \then 0 \
                                    \else - ( maker ( maker , - ( x , 1 ) ) , 4 ) \
         \in let times4 = λ x -> makemult ( makemult , x ) \
             \in times4 ( 3 )"

f = "let makemult = λ maker , x , y -> \
                        \if iszero x \
                        \then 0 \
                        \else - ( maker ( maker , - ( x , 1 ) , y ) , - ( 0 , y ) ) \
     \in let times = λ x , y -> \
                         \makemult ( makemult , x , y ) \
         \in let makefact = λ maker , n -> \
                                \if iszero n \
                                \then 1 \
                                \else times ( maker ( maker , - ( n , 1 ) ) , n ) \
             \in let fact = λ n -> \
                                \makefact ( makefact , n ) \
                 \in fact ( 6 )"


t = "let a = 3 \
     \in let p = λ x -> - ( x , a ) \
         \in let a = 5 \
             \in - ( a , p ( 2 ) )"

{- pari = (Let "even" (ProcE ["n"]
                          (Ifte (Iszero (Var "n"))
                                (Const 1)
                                (CallE (Var "odd")
                                       [Diff (Var "n")
                                             (Const 1)])))
            (Let "odd" (ProcE ["n"]
                              (Ifte (Iszero (Var "n"))
                                    (Const 0)
                                    (CallE (Var "even")
                                           [Diff (Var "n")
                                                 (Const 1)])))
                 (CallE (Var "odd")
                        [Const 13]))) -}

pari = "let even = λ n -> \
                       \if iszero n \
                       \then 1 \
                       \else odd ( - ( n , 1 ) ) \
            \in let odd = λ n -> \
                              \if iszero n \
                              \then 0 \
                              \else even ( - ( n , 1 ) ) \
                \in odd ( 13 )"
