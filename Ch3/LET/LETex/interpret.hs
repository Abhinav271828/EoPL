module Interpret where

import Environment
import Parse

valueofprog :: String -> ExpVal
valueofprog p = valueof (scanparse p) initenv

valueof :: AST -> Env -> ExpVal
valueof (Const  x        ) r = Numv x
valueof (Minus  x        ) r = Numv (-x)
valueof (EmpL            ) r = Listv []
valueof (Cons   a   d    ) r = let e1 = valueof a r
                                   Listv e2 = valueof d r
                               in Listv (e1:e2)
valueof (Car    ls       ) r = let Cons a d = ls
                               in (valueof a r)
valueof (Cdr    ls       ) r = let Cons a d = ls
                               in (valueof d r)
valueof (List   ls       ) r = Listv [valueof l r | l <- ls]
valueof (Var    s        ) r = applyenv r s
valueof (Bin    op  x  y ) r = let Numv v1 = valueof x r
                                   Numv v2 = valueof y r
                                   B    b  = op
                               in Numv (b v1 v2)
valueof (Iszero e        ) r = let Numv v = valueof e r
                               in Boolv (v == 0)
valueof (IsEq   e   f    ) r = let Numv u = valueof e r
                                   Numv v = valueof f r
                               in Boolv (u == v)
valueof (IsLt   e   f    ) r = let Numv u = valueof e r
                                   Numv v = valueof f r
                               in Boolv (u < v)
valueof (IsGt   e   f    ) r = let Numv u = valueof e r
                                   Numv v = valueof f r
                               in Boolv (u > v)
valueof (Let    var e  b ) r = let v = valueof e r
                               in valueof b (extendenv var v r)
valueof (Lets   ls     b ) r = let extpair = \(s,x) e ->
                                                extendenv s
                                                (valueof x r) e
                               in valueof b (foldr extpair r ls)
valueof (Unpack ss  es b ) r = let extpair = \(s,x) e -> 
                                                extendenv s x e
                                   Listv bs = valueof es r
                               in valueof b (foldr extpair r (zip ss bs))
valueof (Ifte   c   t  e ) r = let Boolv v = valueof c r
                               in if v then (valueof t r)
                                  else (valueof e r)
valueof (Cond   cs       ) r = case cs of
                                ((c,t):es) -> valueof (Ifte c t (Cond es)) r
                                []         -> Null
--                     --
