import Data.Char
-- Expressed Values Datatype --
data ExpVal = Numv Int            |
              Boolv Bool          |
              Empv                |
              Listv [ExpVal]      |
              Null
              deriving Show

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
extendenv var val e = \s -> if (s == var) then val else (e s)

applyenv :: Env -> String -> ExpVal
applyenv = ($)

initenv :: Env
initenv = emptyenv
--                      --

-- Expression Datatype --
data AST = Const  Int                     |
           Minus  Int                     |
           EmpL                           |
           Cons   AST             AST     |
           Car    AST                     |
           Cdr    AST                     |
           List   [AST]                   |
           Var    String                  |
           Bin    BOp             AST AST | 
           Iszero AST                     |
           IsEq   AST             AST     |
           IsLt   AST             AST     |
           IsGt   AST             AST     |
           Let    String          AST AST |
           Lets   [(String, AST)] AST     |
           Unpack [String]        AST AST |
           Ifte   AST             AST AST |
           Cond   [(AST, AST)]
           deriving Show

data BOp = B (Int -> Int -> Int)
instance Show BOp where
  show (B b) = show (b 1 1)

valueofprog :: String -> ExpVal
valueofprog p = valueof (scanparse p) initenv

valueof :: AST -> Env -> ExpVal
valueof (Const  x        ) r = Numv x
valueof (Minus  x        ) r = Numv (-x)
valueof (EmpL            ) r = Listv []
valueof (Cons   a   d    ) r = let e1 = valueof a r
                                   Listv e2 = valueof d r
                               in Listv (e1:e2)
valueof (Car    ls       ) r = let Cons a d = ls
                               in (valueof a r)
valueof (Cdr    ls       ) r = let Cons a d = ls
                               in (valueof d r)
valueof (List   ls       ) r = Listv [valueof l r | l <- ls]
valueof (Var    s        ) r = applyenv r s
valueof (Bin    op  x  y ) r = let Numv v1 = valueof x r
                                   Numv v2 = valueof y r
                                   B    b  = op
                               in Numv (b v1 v2)
valueof (Iszero e        ) r = let Numv v = valueof e r
                               in Boolv (v == 0)
valueof (IsEq   e   f    ) r = let Numv u = valueof e r
                                   Numv v = valueof f r
                               in Boolv (u == v)
valueof (IsLt   e   f    ) r = let Numv u = valueof e r
                                   Numv v = valueof f r
                               in Boolv (u < v)
valueof (IsGt   e   f    ) r = let Numv u = valueof e r
                                   Numv v = valueof f r
                               in Boolv (u > v)
valueof (Let    var e  b ) r = let v = valueof e r
                               in valueof b (extendenv var v r)
valueof (Lets   ls     b ) r = let extpair = \(s,x) e ->
                                                extendenv s
                                                (valueof x r) e
                               in valueof b (foldr extpair r ls)
valueof (Unpack ss  es b ) r = let extpair = \(s,x) e -> 
                                                extendenv s x e
                                   Listv bs = valueof es r
                               in valueof b (foldr extpair r (zip ss bs))
valueof (Ifte   c   t  e ) r = let Boolv v = valueof c r
                               in if v then (valueof t r)
                                  else (valueof e r)
valueof (Cond   cs       ) r = case cs of
                                ((c,t):es) -> valueof (Ifte c t (Cond es)) r
                                []         -> Null
--                     --

scanparse :: String -> AST
scanparse prog = let (p, []) = parse (words prog)
                  in p

parse :: [String] -> (AST, [String])
parse lex = case lex of
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
                                           (body, rest) = parse rem
              ("-":"(":ls) -> (Bin (B (-)) op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("+":"(":ls) -> (Bin (B (+)) op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("iszero":ls) -> (Iszero body, rest)
                                  where (body, rest) = parse ls
              ("car":ls) -> (Car body, rest)
                                  where (body, rest) = parse ls
              ("cdr":ls) -> (Cdr body, rest)
                                  where (body, rest) = parse ls
              ("-":x:ls) -> (Minus (read x :: Int), ls)
              ("if":ls) -> (Ifte c t e, rest)
                              where (c, "then":rem) = parse ls
                                    (t, "else":erem) = parse rem
                                    (e, rest) = parse erem
              ("[]":ls) -> (EmpL, ls)
              ("cons":ls) -> (Cons car cdr, rest)
                                where (car, rem) = parse ls
                                      (cdr, rest) = parse rem
              ("==":ls) -> (IsEq op1 op2, rest)
                                where (op1, rem) = parse ls
                                      (op2, rest) = parse rem
              ("<":ls) -> (IsLt op1 op2, rest)
                                where (op1, rem) = parse ls
                                      (op2, rest) = parse rem
              (">":ls) -> (IsGt op1 op2, rest)
                                where (op1, rem) = parse ls
                                      (op2, rest) = parse rem
              ("lets":ls) -> (Lets assm body, rest)
                                where (assm,"in":rem) = parselets ls
                                      (body, rest) = parse rem
                                      parselets (x:"=":xs) = ((x,val):assms , s)
                                                              where (val, r) = parse xs
                                                                    (assms,s) = case r of 
                                                                          ",":rs -> parselets rs
                                                                          "in":rs -> ([],r)
              ("list":"(":ls) -> (List es, rest)
                                where (es, rest) = parselist ls
                                      parselist exps = (e:r, rem)
                                                        where (e, s) = parse exps
                                                              (r, rem) = case s of
                                                                         ",":rs -> parselist rs
                                                                         ")":rs -> ([],rs)
              ("unpack":"(":ls) -> (Unpack vars val body, rest)
                                   where (vars, "=":rem) = parsestrs ls
                                         (val, "in":r) = parse rem
                                         (body, rest) = parse r
                                         parsestrs vs = (v:xv, s)
                                                        where (v:xs) = vs
                                                              (xv,s) = case xs of
                                                                        ",":rs -> parsestrs rs
                                                                        ")":rs -> ([],rs)
              ("cond":"(":ls) -> (Cond cs, rest)
                              where (cs,rest) = parseconds ls
                                    parseconds conds = ((c,t):rs, rem)
                                                        where (c, "->":r) = parse conds
                                                              (t, s) = parse r
                                                              (rs, rem) = case s of
                                                                           ",":xs -> parseconds xs
                                                                           ")":xs -> ([], xs)

              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else (Var l, ls)

prog :: AST -- evaluates to Numv (-5)
prog = (Let "x" (Const 7)
            (Let "y" (Const 2)
                 (Let "y" (Let "x" (Bin (B (-)) (Var "x")
                                                (Const 1))
                               (Bin (B (-)) (Var "x")
                                            (Var "y")))
                      (Bin (B (-)) (Bin (B (-)) (Var "x")
                                            (Const 8))
                                   (Var "y")))))
