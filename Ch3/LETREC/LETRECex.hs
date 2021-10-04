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

valueofprog :: AST -> ExpVal
valueofprog p = valueof p initenv

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

prog :: AST -- evaluates to Numv 12
prog = (Letrec [("double", ["x"], (Ifte (Iszero (Var "x"))
                                        (Const 0)
                                        (Diff (CallE (Var "double")
                                                     [Diff (Var "x")
                                                           (Const 1)])
                                              (Const (-2)))))]
               (CallE (Var "double")
                      [Const 6]))

prod = (Letrec [("mult", ["x","y"], (Ifte (Iszero (Var "y"))
                                          (Const 0)
                                          (Diff (CallE (Var "mult")
                                                       [Var "x",
                                                        Diff (Var "y")
                                                             (Const 1)])
                                                (Diff (Const 0)
                                                      (Var "x")))))]
               (CallE (Var "mult")
                      [Const 13,
                       Const 17]))

pari = (Letrec [("even", ["x"], (Ifte (Iszero (Var "x"))
                                      (Const 1)
                                      (CallE (Var "odd")
                                             [Diff (Var "x")
                                                   (Const 1)]))),
                ("odd",  ["x"], (Ifte (Iszero (Var "x"))
                                      (Const 0)
                                      (CallE (Var "even")
                                             [Diff (Var "x")
                                                   (Const 1)])))]
               (CallE (Var "odd")
                      [Const 13]))

rep = (Letrec [("rep", ["x"], (Cons (Var "x")
                                   (CallE (Var "rep")
                                          [Var "x"]))),
                ("take", ["n","l"], (Ifte (Iszero (Var "n"))
                                          (EmpL)
                                          (Cons (Car (Var "l"))
                                                (CallE (Var "take")
                                                       [Diff (Var "n")
                                                             (Const 1),
                                                        (Cdr (Var "l"))]))))]
              (CallE (Var "take")
                     [Const 5,
                      CallE (Var "rep")
                            [Const 7]]))
