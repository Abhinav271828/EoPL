module Interpret where

import Parse
import Translation

-- Procedure Datatype (Prelim) --
type Proc = ExpVal -> ExpVal

proc :: NLAST -> Env -> Proc
proc b r = \e -> valueof b (extendenv e r)

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
type Env = [ExpVal]

emptyenv :: Env
emptyenv = []

extendenv :: ExpVal -> Env -> Env
extendenv = (:)

applyenv :: Env -> Int -> ExpVal
applyenv (x:_)  0 = x
applyenv (_:xs) n = applyenv xs (n-1)

initenv :: Env
initenv = emptyenv
--                      --

valueofprog :: String -> ExpVal
valueofprog p = valueof (translationofprog $ scanparse p) initenv

valueof :: NLAST -> Env -> ExpVal
valueof (NLConst  x         ) r = Numv x
valueof (NLVar    x         ) r = applyenv r x
valueof (NLDiff   e1  e2    ) r = let Numv v1 = valueof e1 r
                                      Numv v2 = valueof e2 r
                                  in Numv (v1 - v2)
valueof (NLIszero e         ) r = let Numv v = valueof e r
                                  in Boolv (v == 0)
valueof (NLLet    e   b     ) r = let v = valueof e r
                                  in valueof b (extendenv v r)
valueof (NLIfte   c   t   e ) r = let Boolv v = valueof c r
                                  in if v then (valueof t r)
                                     else (valueof e r)
valueof (NLProc  b          ) r = Procv (proc b r)
valueof (NLCall  rat ran    ) r = let Procv fun = valueof rat r
                                      arg       = valueof ran r
                                  in applyproc fun arg
--                     --
