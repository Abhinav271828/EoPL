module Interpret where

import Parse
import Translation

-- Procedure Datatype (Prelim) --
type Proc = [ExpVal] -> ExpVal

proc :: NLAST -> Env -> Proc
proc b r = \es -> valueof b (extendenv es r)

applyproc :: Proc -> [ExpVal] -> ExpVal
applyproc = ($)
--                             --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Listv [ExpVal] | Null

instance Show ExpVal where
  show (Numv  n) = "Numv "  ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p [Numv 1])
  show (Listv l) = "Listv " ++ show l

getnum :: ExpVal -> Int
getnum (Numv x) = x
getbool :: ExpVal -> Bool
getbool (Boolv x) = x
getproc :: ExpVal -> Proc
getproc (Procv x) = x
--                           --

-- Environment Datatype --
type Env = [[ExpVal]]

emptyenv :: Env
emptyenv = []

extendenv :: [ExpVal] -> Env -> Env
extendenv = (:)

extendenvrec :: [NLAST] -> Env -> Env
extendenvrec es r = s
                     where s = extendenv (map (\e -> Procv (proc e s)) es) r

applyenv :: Env -> (Int,Int) -> ExpVal
applyenv r (x, y) = r !! x !! y

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
valueof (NLCons   e1  e2    ) r = let v1 = valueof e1 r
                                      Listv v2 = valueof e2 r
                                  in Listv (v1:v2)
valueof (NLEmpL             ) r  = Listv []
valueof (NLCar    e         ) r = let Listv (v:_)  = valueof e r
                                  in v
valueof (NLCdr    e         ) r = let Listv (_:vs) = valueof e r
                                  in Listv vs
valueof (NLIszero e         ) r = let Numv v = valueof e r
                                  in Boolv (v == 0)
valueof (NLLets   es   b    ) r = valueof b (extendenv (map (\e -> valueof e r) es) r)
valueof (NLLetrec es   b    ) r = valueof b (extendenvrec es r)
valueof (NLIfte   c    t   e) r = let Boolv v = valueof c r
                                  in if v then (valueof t r)
                                     else (valueof e r)
valueof (NLCond   cs        ) r = case cs of
                                     ((c,t):es) -> valueof (NLIfte c t (NLCond es)) r
                                     [] -> Null
valueof (NLProc   b         ) r = Procv (proc b r)
valueof (NLCall   rat ran   ) r = let args = map (\a -> valueof a r) ran
                                  in case rat of
                                       NLProc fun -> valueof fun (extendenv args r)
                                       _ -> let Procv fun = valueof rat r
                                            in applyproc fun args
--                     --
