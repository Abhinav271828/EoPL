module Interpret where

import Parse
import Data.Char

-- Procedure Datatype (Prelim) --
type Proc = [ExpVal] -> Store -> (ExpVal, Store)

proc :: [String] -> AST -> Env -> Proc
proc vs b r = \es s -> valueof b (foldr ($) r
                                    [extendenv v e
                                       | (v,e) <- zip vs es])
                               s
applyproc :: Proc -> [ExpVal] -> Store -> (ExpVal, Store)
applyproc = id
--                             --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Listv [ExpVal] | Refv Int | Null

instance Show ExpVal where
  show (Numv  n) = "Numv " ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p [Refv 0] [Numv 2])
  show (Listv l) = show l
  show (Refv  r) = "Refv " ++ show r
  show (Null)    = "Null"

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
newref val s = (length s, s ++ [val])

newrefs :: [a] -> Store -> ([Int], Store)
newrefs [] s = ([], s)
newrefs (f:fs) s = let (i, s1) = newref Null s
                       (is, s2) = newrefs fs s1
                   in (i:is, s2)

deref :: Int -> Store -> ExpVal
deref i s = s !! i

setref :: Int -> ExpVal -> Store -> Store
setref i val s = (take i s) ++ (val : (drop (i+1) s))

setrefs :: [(String, [String], AST)] -> (Env, Store) -> Store
setrefs [] (r,s)                 = s
setrefs ((fname, vs, b):fs) (r,s) = setrefs fs (r, setref addr (Procv $ proc vs b r) s)
                                      where Refv addr = applyenv r fname
--                --


-- Environment Datatype --
type Env = [(String, ExpVal)]

emptyenv :: Env
emptyenv = []

extendenv :: String -> ExpVal -> Env -> Env
extendenv var val e = (var, val) : e
                                 
extendenvrec :: [(String, [String], AST)] -> (Env, Store) -> (Env, Store)
extendenvrec fs (e,s) = (r, s2)
                        where (refs, s1) = newrefs fs s
                              r = foldr ($) e [extendenv fname (Refv ref) | ((fname,_,_), ref) <- zip fs refs ]
                              s2 = setrefs fs (r,s1)

applyenv :: Env -> String -> ExpVal
applyenv e s = let Just val = lookup s e
               in val

initenv :: Env
initenv = emptyenv
--                      --

valueofprog :: String -> ExpVal
valueofprog p = fst $ valueof (scanparse p) initenv emptystore

valueof :: AST -> Env -> Store -> (ExpVal, Store)
valueof (Const  x           ) r s = (Numv x, s)
valueof (RefE   i           ) r s = (Refv i, s)
valueof (Assign x   b       ) r s = let (exp, s1) = valueof b r s
                                        Refv i = applyenv r x
                                    in (Numv 42, setref i exp s1)
valueof (Cons   a   d       ) r s = let (v1, s1) = valueof a r s
                                        (Listv v2, s2) = valueof d r s1
                                    in (Listv (v1:v2) , s2)
valueof (Car    l           ) r s = let (Listv v, s1) = valueof l r s
                                    in (head v, s1)
valueof (Cdr    l           ) r s = let (Listv v, s1) = valueof l r s
                                    in (Listv (tail v), s1)
valueof (EmpL               ) r s = (Listv [], s)
valueof (Var    v           ) r s = case (applyenv r v) of
                                      Refv i -> (deref i s, s)
                                      e -> (e, s)
valueof (Diff   e1  e2      ) r s = let (Numv v1, s1) = valueof e1 r s
                                        (Numv v2, s2) = valueof e2 r s1
                                    in (Numv (v1 - v2), s2)
valueof (Iszero e           ) r s = let (Numv v, s1) = valueof e r s
                                    in (Boolv (v == 0), s1)
valueof (LetM   var e   b   ) r s = let (val, s1) = valueof e r s
                                        (i, s2) = newref val s1
                                    in valueof b (extendenv var (Refv i) r) s2
valueof (Let    var e   b   ) r s = let (val, s1) = valueof e r s
                                    in valueof b (extendenv var val r) s1
valueof (SetD   var e   b   ) r s = let Refv i = applyenv r var
                                        (val, s1) = valueof e r s
                                        s2 = setref i val s1
                                        (ret, _) = valueof b r s2
                                    in (ret, s)
valueof (Letrec fns b       ) r s = let (r1, s1) = extendenvrec fns (r,s)
                                    in valueof b r1 s1
valueof (Ifte   c   t   e   ) r s = let (Boolv v, s1) = valueof c r s
                                    in if v
                                       then (valueof t r s1)
                                       else (valueof e r s1)
valueof (ProcE  vs  b       ) r s = (Procv (proc vs b r), s)
valueof (CallE  rat ran     ) r s = let (Procv fun, s1) = valueof rat r s
                                        (args, s2) = valueofargs ran r s1
                                    in applyproc fun args s2
                                        where valueofargs [] _ s = ([],s)
                                              valueofargs (a:as) r s = let (v, s1) = valueof a r s
                                                                           (vs, s2) = valueofargs as r s1
                                                                       in (v:vs, s2)
valueof (Seq    exps        ) r s = let (v, s1) = valueof (head exps) r s
                                    in case (tail exps) of
                                        [] -> (v,s1)
                                        _  -> valueof (Seq (tail exps)) r s1
--                     --
