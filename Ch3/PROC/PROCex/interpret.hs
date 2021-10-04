module Interpret where

import Parse

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
