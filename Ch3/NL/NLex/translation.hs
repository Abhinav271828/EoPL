module Translation where

import Parse

-- Expression Datatype --
data NLAST = NLConst Int              |
             NLVar (Int,Int)                |
             NLDiff NLAST NLAST       |
             NLCons NLAST NLAST | NLEmpL |
             NLCar NLAST | NLCdr NLAST |
             NLIszero NLAST           |
             NLLets [NLAST] NLAST     |
             NLLetrec [NLAST] NLAST     |
             NLIfte NLAST NLAST NLAST |
             NLCond [(NLAST,NLAST)]   |
             NLProc NLAST             |
             NLCall NLAST [NLAST]
             deriving Show

-- Static Environments --
type Senv = [[String]]

emptysenv :: Senv
emptysenv = []

extendsenv :: [String] -> Senv -> Senv
extendsenv = (:)

applysenv :: Senv -> String -> (Int, Int)
applysenv (s:ss) v = if (v `elem` s) then (0, lookup v s) else ((\(a,b) -> (1+a,b)) (applysenv ss v))
                        where lookup var (x:xs) = if x == var then 0 else (1 + (lookup var xs))

initsenv :: Senv
initsenv = emptysenv
--                     --

translationofprog :: AST -> NLAST
translationofprog p = translationof p initsenv

translationof :: AST -> Senv -> NLAST
translationof (Const x) s = NLConst x
translationof (Var x) s = NLVar (applysenv s x)
translationof (Diff e1 e2) s = NLDiff (translationof e1 s) (translationof e2 s)
translationof (Cons e1 e2) s = NLCons (translationof e1 s) (translationof e2 s)
translationof (EmpL) s = NLEmpL
translationof (Car e) s = NLCar (translationof e s)
translationof (Cdr e) s = NLCdr (translationof e s)
translationof (Iszero e) s = NLIszero (translationof e s)
translationof (Ifte c t e) s = NLIfte (translationof c s) (translationof t s) (translationof e s)
translationof (Cond es) s = NLCond (map (\(i,e) -> (translationof i s, translationof e s)) es)
translationof (Lets es b) s = NLLets (map (\(_,e) -> translationof e s) es) (translationof b (extendsenv (map fst es) s))
translationof (Letrec fs b) s = NLLetrec
                                      (map (\(v,as,e) ->
                                                translationof e (extendsenv as names))
                                           fs)
                                      (translationof b names)
                                                where names = extendsenv (map (\(v,_,_) -> v) fs) s
translationof (ProcE vs b) s = NLProc (translationof b (extendsenv vs s))
translationof (CallE f as) s = NLCall (translationof f s) (map (\a -> translationof a s) as)
