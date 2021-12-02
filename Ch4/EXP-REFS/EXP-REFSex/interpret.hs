module Interpret where

import Parse
import Data.Char

-- Procedure Datatype (Prelim) --
type Proc = [ExpVal] -> Store -> (ExpVal, Store)

proc :: [String] -> AST -> Env -> Proc
proc vs b r = \es s -> valueof b (foldr (\(v,e) env ->
                                            extendenv v e env)
                                        r
                                        (zip vs es)) s

applyproc :: Proc -> [ExpVal] -> Store -> (ExpVal, Store)
applyproc = id
--                             --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Listv [ExpVal] | Refv Int | Null

instance Show ExpVal where
  show (Numv  n) = "Numv " ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p [Numv 1] emptystore)
  show (Listv l) = show l

getnum :: ExpVal -> Int
getnum (Numv x) = x
getbool :: ExpVal -> Bool
getbool (Boolv x) = x
getproc :: ExpVal -> Proc
getproc (Procv x) = x
--                           --

-- Store Datatype --
type Store = [ExpVal]

emptystore :: Store
emptystore = []

newref :: ExpVal -> Store -> (Int, Store)
newref val s = (length s, val:s)

deref :: Int -> Store -> ExpVal
deref i s = (reverse s) !! i

setref :: Int -> ExpVal -> Store -> Store
setref i val s = let r = reverse s
                 in reverse $ (take i r) ++ (val : (drop (i+1) r))
--                --


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
valueofprog p = fst $ valueof (scanparse p) initenv emptystore

valueof :: AST -> Env -> Store -> (ExpVal, Store)
valueof (Const  x           ) r s = (Numv x, s)
valueof (RefE   i           ) r s = (Refv i, s)
valueof (Cons   a   d       ) r s = let (v1, s1) = valueof a r s
                                        (Listv v2, s2) = valueof d r s1
                                    in (Listv (v1:v2) , s2)
valueof (Car    l           ) r s = let (Listv v, s1) = valueof l r s
                                    in (head v, s1)
valueof (Cdr    l           ) r s = let (Listv v, s1) = valueof l r s
                                    in (Listv (tail v), s1)
valueof (EmpL               ) r s = (Listv [], s)
valueof (Var    v           ) r s = (applyenv r v, s)
valueof (Diff   e1  e2      ) r s = let (Numv v1, s1) = valueof e1 r s
                                        (Numv v2, s2) = valueof e2 r s1
                                    in (Numv (v1 - v2), s2)
valueof (Iszero e           ) r s = let (Numv v, s1) = valueof e r s
                                    in (Boolv (v == 0), s1)
valueof (Let    var e   b   ) r s = let (v, s1) = valueof e r s
                                    in valueof b (extendenv var v r) s1
valueof (Letrec procs b     ) r s = valueof b (extendenvrec procs r) s
valueof (Ifte   c   t   e   ) r s = let (Boolv v, s1) = valueof c r s
                                    in if v then (valueof t r s1)
                                       else (valueof e r s1)
valueof (ProcE  vs  b       ) r s = (Procv (proc vs b r), s)
valueof (CallE  rat ran     ) r s = let (Procv fun, s1) = valueof rat r s
                                        (args, s2) = valueofargs ran r s1
                                    in applyproc fun args s2
                                        where valueofargs [] _ s = ([],s)
                                              valueofargs (a:as) r s = let (v, s1) = valueof a r s
                                                                           (vs, s2) = valueofargs as r s1
                                                                       in (v:vs, s2)
valueof (Newref exp         ) r s = let (val, s1) = valueof exp r s
                                        (i, s2) = newref val s1
                                    in (Refv i, s2)
valueof (Deref  ref         ) r s = let (Refv i, s1) = valueof ref r s
                                    in (deref i s1, s1)
valueof (Setref ref exp     ) r s = let (Refv i, s1) = valueof ref r s
                                        (val, s2) = valueof exp r s
                                    in (val , setref i val s2)       -- Exercise 4.6
                                 -- in (deref i s1, setref i val s2) -- Exercise 4.7
valueof (Seq    exps        ) r s = let (v, s1) = valueof (head exps) r s
                                    in case (tail exps) of
                                        [] -> (v,s1)
                                        _  -> valueof (Seq (tail exps)) r s1
--                     --
