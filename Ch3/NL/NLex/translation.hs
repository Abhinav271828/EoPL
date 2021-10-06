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
-- translationof (Lets es b) s = NLLets (map (\(_,e) -> translationof e s) es) (translationof b (extendsenv (map fst es) s))
translationof (Letrec fs b) s = NLLetrec
                                      (map (\(v,as,e) ->
                                                translationof e (extendsenv as names))
                                           fs)
                                      (translationof b names)
                                                where names = extendsenv (map (\(v,_,_) -> v) fs) s
translationof (ProcE vs b) s = NLProc (translationof b (extendsenv vs s))
translationof (CallE f as) s = NLCall (translationof f s) (map (\a -> translationof a s) as)
translationof (Lets es b) s = let functions = filter (\(_,e) -> isProc e) es
                                  variables = filter (\(_,e) -> not $ isProc e) es
                                  body = replace functions b
                              in NLLets (map (\(_,e) -> translationof e s) variables) (translationof body
                                                                                         (extendsenv (map fst variables) s))

isProc :: AST -> Bool
isProc p = case p of
             ProcE _ _ -> True
             _ -> False

replace :: [(String,AST)] -> AST -> AST
replace [] p = p
replace l@((n,val):fs) p = let prog = replace fs p
                               ProcE args body = val
                           in case prog of
                                Var n -> val
                                Diff e1 e2 -> Diff (replace l e1) (replace l e2)
                                Cons e1 e2 -> Cons (replace l e1) (replace l e2)
                                Car e -> Car (replace l e)
                                Cdr e -> Cdr (replace l e)
                                Iszero e -> Iszero (replace l e)
                                Lets xs b -> Lets (map (\(x,y) -> (x, replace l y)) xs) (replace l b)
                                Letrec xs b -> Letrec (map (\(x,y,z) -> (x, y, replace l z)) xs) (replace l b)
                                Ifte c t e -> Ifte (replace l c) (replace l t) (replace l e)
                                Cond cs -> Cond (map (\(x,y) -> (replace l x, replace l y)) cs)
                                ProcE as b -> ProcE as (replace l b)
                                CallE f as -> CallE (replace l f) (map (replace l) as)
                                _ -> prog
