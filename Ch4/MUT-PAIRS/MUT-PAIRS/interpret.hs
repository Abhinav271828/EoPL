module Interpret where

import Parse
import Data.Char

-- Procedure Datatype (Prelim) --
type Proc = ExpVal -> Store -> (ExpVal, Store)

proc :: String -> AST -> Env -> Proc
proc v b r = \e s -> let (i, s1) = (newref e s)
                     in valueof b (extendenv v (Refv i) r) s1

applyproc :: Proc -> ExpVal -> Store -> (ExpVal, Store)
applyproc = id
--                             --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Refv Int | Pairv Int | Null

instance Show ExpVal where
  show (Numv  n) = "Numv " ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p (Refv 0) [Numv 2])
  show (Refv  r) = "Refv " ++ show r
  show (Pairv i) = "Pairv " ++ show i ++ " " ++ show (i+1)
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

deref :: Int -> Store -> ExpVal
deref i s = s !! i

setref :: Int -> ExpVal -> Store -> Store
setref i val s = (take i s) ++ (val : (drop (i+1) s))
--                --

-- Pair Manipulation --
newpair :: ExpVal -> ExpVal -> Store -> (ExpVal, Store)
newpair e1 e2 s = let (i, s1) = newref e1 s
                      (_, s2) = newref e2 s1
                  in (Pairv i, s2)

left :: ExpVal -> Store -> ExpVal
left (Pairv i) s = deref i s
right :: ExpVal -> Store -> ExpVal
right (Pairv i) s = deref (i+1) s

setleft :: ExpVal -> ExpVal -> Store -> Store
setleft (Pairv i) e s = setref i e s
setright :: ExpVal -> ExpVal -> Store -> Store
setright (Pairv i) e s = setref (i+1) e s
--                   --


-- Environment Datatype --
type Env = [(String, ExpVal)]

emptyenv :: Env
emptyenv = []

extendenv :: String -> ExpVal -> Env -> Env
extendenv var val e = (var, val) : e
                                 
extendenvrec :: [(String, String, AST)] -> (Env, Store) -> (Env, Store)
extendenvrec fs (e,s) = (r, s2)
                        where (refs, s1) = newrefs fs s
                              r = foldr ($) e [extendenv fname (Refv ref) | ((fname,_,_), ref) <- zip fs refs ]
                              s2 = setrefs fs (r,s1)

newrefs :: [a] -> Store -> ([Int], Store)
newrefs [] s = ([], s)
newrefs (f:fs) s = let (i, s1) = newref Null s
                       (is, s2) = newrefs fs s1
                   in (i:is, s2)

setrefs :: [(String, String, AST)] -> (Env, Store) -> Store
setrefs [] (r,s)                 = s
setrefs ((fname, v, b):fs) (r,s) = setrefs fs (r, setref addr (Procv $ proc v b r) s)
                                      where Refv addr = applyenv r fname

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
valueof (Var    v           ) r s = let Refv i = applyenv r v
                                    in (deref i s, s)
valueof (NewPair e1 e2      ) r s = let (v1, s1) = valueof e1 r s
                                        (v2, s2) = valueof e2 r s1
                                    in newpair v1 v2 s2
valueof (LeftE  p           ) r s = let (Pairv i, s1) = valueof p r s
                                    in (deref i s1, s1)
valueof (RightE p           ) r s = let (Pairv i, s1) = valueof p r s
                                    in (deref (i+1) s1, s1)
valueof (SetLeft x  e       ) r s = let (v, s1) = valueof e r s
                                        Pairv i = applyenv r x
                                    in (Numv 42, setref i v s1)
valueof (SetRight x  e      ) r s = let (v, s1) = valueof e r s
                                        Pairv i = applyenv r x
                                    in (Numv 42, setref (i+1) v s1)
valueof (Diff   e1  e2      ) r s = let (Numv v1, s1) = valueof e1 r s
                                        (Numv v2, s2) = valueof e2 r s1
                                    in (Numv (v1 - v2), s2)
valueof (Iszero e           ) r s = let (Numv v, s1) = valueof e r s
                                    in (Boolv (v == 0), s1)
valueof (Let    var e   b   ) r s = let (val, s1) = valueof e r s
                                        (i, s2) = newref val s1
                                    in valueof b (extendenv var (Refv i) r) s2
valueof (Letrec fns b       ) r s = let (r1, s1) = extendenvrec fns (r,s)
                                    in valueof b r1 s1
valueof (Ifte   c   t   e   ) r s = let (Boolv v, s1) = valueof c r s
                                    in if v
                                       then (valueof t r s1)
                                       else (valueof e r s1)
valueof (ProcE  v   b       ) r s = (Procv (proc v b r), s)
valueof (CallE  rat ran     ) r s = let (Procv fun, s1) = valueof rat r s
                                        (arg, s2) = valueof ran r s1
                                    in applyproc fun arg s2
valueof (Seq    exps        ) r s = let (v, s1) = valueof (head exps) r s
                                    in case (tail exps) of
                                        [] -> (v,s1)
                                        _  -> valueof (Seq (tail exps)) r s1
--                     --
