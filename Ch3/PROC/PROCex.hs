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
           ProcE [String] AST   |
           CallE AST [AST]
           deriving Show

valueofprog :: AST -> ExpVal
valueofprog p = valueof p initenv

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

prog :: AST -- evaluates to Numv (-100)
prog = (Let "x" (Const 200)
            (Let "f" (ProcE ["z"]
                       (Diff (Var "z")
                             (Var "x")))
                 (Let "x" (Const 100)
                      (Let "g" (ProcE ["z"]
                                 (Diff (Var "z")
                                       (Var "x")))
                           (Diff (CallE (Var "f") [Const 1])
                                 (CallE (Var "g") [Const 1]))))))

p = (Let "makemult" (ProcE ["maker","x"]
                           (Ifte (Iszero (Var "x"))
                                 (Const 0)
                                 (Diff (CallE (Var "maker")
                                              [Var "maker",
                                               Diff (Var "x")
                                                    (Const 1)])
                                       (Const (-4)))))
          (Let "times4" (ProcE ["x"]
                               (CallE (Var "makemult")
                                      [Var "makemult",
                                       Var "x"]))
               (CallE (Var "times4")
                      [Const 3])))

f = (Let "makemult" (ProcE ["maker","x","y"]
                           (Ifte (Iszero (Var "x"))
                                 (Const 0)
                                 (Diff (CallE (Var "maker")
                                              [Var "maker",
                                               Diff (Var "x")
                                                    (Const 1),
                                               Var "y"])
                                       (Diff (Const 0)
                                             (Var "y")))))
          (Let "times" (ProcE ["x","y"]
                              (CallE (Var "makemult")
                                     [Var "makemult",
                                      Var "x",
                                      Var "y"]))
               (Let "makefact" (ProcE ["maker","n"]
                                     (Ifte (Iszero (Var "n"))
                                           (Const 1)
                                           (CallE (Var "times")
                                                  [CallE (Var "maker")
                                                         [Var "maker",
                                                          Diff (Var "n")
                                                               (Const 1)],
                                                   Var "n"])))
                    (Let "fact" (ProcE ["n"]
                                      (CallE (Var "makefact")
                                             [Var "makefact",
                                              Var "n"]))
                         (CallE (Var "fact")
                                [Const 6])))))

t = (Let "a" (Const 3)
         (Let "p" (ProcE ["x"]
                         (Diff (Var "x")
                               (Var "a")))
              (Let "a" (Const 5)
                   (Diff (Var "a")
                         (CallE (Var "p")
                                [Const 2])))))

pari = (Let "even" (ProcE ["n"]
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
                        [Const 13])))
