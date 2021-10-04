module Interpret where

import Parse

-- Procedure Datatype (Prelim) --
type Proc = ExpVal -> ExpVal

proc :: String -> AST -> Env -> Proc
proc var b r = \e -> valueof b (extendenv var e r)

applyproc :: Proc -> ExpVal -> ExpVal
applyproc = ($)
--                             --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Null

instance Show ExpVal where
  show (Numv  n) = "Numv " ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p (Numv 1))

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

extendenvrec :: String -> String -> AST -> Env -> Env
extendenvrec pname bvar prc e = r
                                where r = \s -> if (s == pname)
                                                then Procv (proc bvar prc r)
                                                else (e s)

applyenv :: Env -> String -> ExpVal
applyenv = ($)

initenv :: Env
initenv = emptyenv
--                      --

valueofprog :: String -> ExpVal
valueofprog p = valueof (scanparse p) initenv

valueof :: AST -> Env -> ExpVal
valueof (Const  x           ) r = Numv x
valueof (Var    s           ) r = applyenv r s
valueof (Diff   e1  e2      ) r = let Numv v1 = valueof e1 r
                                      Numv v2 = valueof e2 r
                                  in Numv (v1 - v2)
valueof (Iszero e           ) r = let Numv v = valueof e r
                                  in Boolv (v == 0)
valueof (Let    var e   b   ) r = let v = valueof e r
                                  in valueof b (extendenv var v r)
valueof (Letrec prc bv  pb b) r = valueof b (extendenvrec prc bv pb r)
valueof (Ifte   c   t   e   ) r = let Boolv v = valueof c r
                                  in if v then (valueof t r)
                                     else (valueof e r)
valueof (ProcE  var b       ) r = Procv (proc var b r)
valueof (CallE  rat ran     ) r = let Procv fun = valueof rat r
                                      arg = valueof ran r
                                  in applyproc fun arg
--                     --
