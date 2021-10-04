module Interpret where

import Environment
import Parse

valueofprog :: String -> ExpVal
valueofprog p = valueof (scanparse p) initenv

valueof :: AST -> Env -> ExpVal
valueof (Const  x      ) r = Numv x
valueof (Var    s      ) r = applyenv r s
valueof (Diff   e1  e2 ) r = let Numv v1 = valueof e1 r
                                 Numv v2 = valueof e2 r
                             in Numv (v1 - v2)
valueof (Iszero e      ) r = let Numv v = valueof e r
                             in Boolv (v == 0)
valueof (Let    var e b) r = let v = valueof e r
                             in valueof b (extendenv var v r)
valueof (Ifte   c   t e) r = let Boolv v = valueof c r
                             in if v then (valueof t r)
                                else (valueof e r)
--                     --
