import Data.Char
-- Procedure Datatype (Prelim) --
type Proc = [ExpVal] -> ExpVal

proc :: [String] -> AST -> Env -> Proc
proc vs b r = \es -> valueof b (foldr (\(v,e) env ->
                                          extendenv v e env)
                                      r
                                      (zip vs es))

applyproc :: Proc -> [ExpVal] -> ExpVal
applyproc = ($)
--                             --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Listv [ExpVal] | Null

instance Show ExpVal where
  show (Numv  n) = "Numv " ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p [Numv 1])
  show (Listv l) = show l

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

extendenvrec :: [(String, [String], AST)] -> Env -> Env
extendenvrec funs e = r
                      where r = \s -> case (filter (\(x,_,_) -> x == s) funs) of
                                        [(p,vs,b)] -> Procv (proc vs b r)
                                        []         -> (e s)

applyenv :: Env -> String -> ExpVal
applyenv = ($)

initenv :: Env
initenv = emptyenv
--                      --

-- Expression Datatype --
data AST = Const Int                    |
           Cons AST AST | EmpL          |
           Car AST | Cdr AST            |
           Var String                   |
           Diff AST AST                 |
           Iszero AST                   |
           Let String AST AST           |
           Letrec [(String, [String], AST)] AST |
           Ifte AST AST AST             |
           ProcE [String] AST             |
           CallE AST [AST]
           deriving Show

valueofprog :: String -> ExpVal
valueofprog p = valueof (scanparse p) initenv

valueof :: AST -> Env -> ExpVal
valueof (Const  x           ) r = Numv x
valueof (Cons   a   d       ) r = let v1 = valueof a r
                                      Listv v2 = valueof d r
                                  in Listv (v1:v2)
valueof (Car    l           ) r = let Listv v = valueof l r
                                  in (head v)
valueof (Cdr    l           ) r = let Listv v = valueof l r
                                  in Listv (tail v)
valueof (EmpL               ) r = Listv []
valueof (Var    s           ) r = applyenv r s
valueof (Diff   e1  e2      ) r = let Numv v1 = valueof e1 r
                                      Numv v2 = valueof e2 r
                                  in Numv (v1 - v2)
valueof (Iszero e           ) r = let Numv v = valueof e r
                                  in Boolv (v == 0)
valueof (Let    var e   b   ) r = let v = valueof e r
                                  in valueof b (extendenv var v r)
valueof (Letrec procs b) r = valueof b (extendenvrec procs r)
valueof (Ifte   c   t   e   ) r = let Boolv v = valueof c r
                                  in if v then (valueof t r)
                                     else (valueof e r)
valueof (ProcE  vs  b       ) r = Procv (proc vs b r)
valueof (CallE  rat ran     ) r = let Procv fun = valueof rat r
                                      arg = [valueof a r | a <- ran]
                                  in applyproc fun arg
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
              ("[]":ls) -> (EmpL, ls)
              ("cons":ls) -> (Cons car cdr, rest)
                                where (car, rem) = parse ls
                                      (cdr, rest) = parse rem
              ("car":ls) -> (Car body, rest)
                                  where (body, rest) = parse ls
              ("cdr":ls) -> (Cdr body, rest)
                                  where (body, rest) = parse ls
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
              ("letrec":ls) -> (Letrec funs body, rest)
                               where (funs, "in":rem) = parsefuns ls
                                     (body, rest) = parse rem
                                     parsefuns fs = ((x,as,b):r, rs)
                                                    where (x:"=":"λ":args) = fs
                                                          (as, "->":s) = parseargs args
                                                          (b, t) = parse s
                                                          (r, rs) = case t of
                                                                     ",":ts -> parsefuns ts
                                                                     "in":_ -> ([],t)
                                                          parseargs bs = (b:u, v)
                                                                         where (b:y) = bs
                                                                               (u,v) = case y of
                                                                                        ",":ys -> parseargs ys
                                                                                        "->":_ -> ([],y)
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


prog = "letrec double = λ x -> if iszero x \
                               \then 0 \
                               \else - ( double ( - ( x , 1 ) ) , - ( 0 , 2 ) ) \
        \in double ( 6 )"

prod = "letrec mult = λ x , y -> if iszero y \
                                 \then 0 \
                                 \else - ( mult ( x , - ( y , 1 ) ) , - ( 0 , x ) ) \
        \in mult ( 13 , 17 )"

pari = "letrec even = λ x -> if iszero x \
                             \then 1 \
                             \else odd ( - ( x , 1 ) ) , \
              \odd = λ x -> if iszero x \
                            \then 0 \
                            \else even ( - ( x , 1 ) ) \
        \in odd ( 13 )"

rep = "letrec rep = λ x -> cons x rep ( x ) , \
             \take = λ n , l -> if iszero n \
                                \then [] \
                                \else cons car l take ( - ( n , 1 ) , l ) \
       \in take ( 5 , rep ( 7 ) )"
