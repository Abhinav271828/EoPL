module Interpret where

import Parse
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
valueof (Letrec procs b.    ) r = valueof b (extendenvrec procs r)
valueof (Ifte   c   t   e   ) r = let Boolv v = valueof c r
                                  in if v then (valueof t r)
                                     else (valueof e r)
valueof (ProcE  vs  b       ) r = Procv (proc vs b r)
valueof (CallE  rat ran     ) r = let Procv fun = valueof rat r
                                      arg = [valueof a r | a <- ran]
                                  in applyproc fun arg
--                     --
