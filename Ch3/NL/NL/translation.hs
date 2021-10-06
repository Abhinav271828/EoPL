module Translation where

import Parse

-- Expression Datatype --
data NLAST = NLConst Int              |
             NLVar Int                |
             NLDiff NLAST NLAST       |
             NLIszero NLAST           |
             NLLet NLAST NLAST        |
             NLIfte NLAST NLAST NLAST |
             NLProc NLAST             |
             NLCall NLAST NLAST
             deriving Show

-- Static Environments --
type Senv = [String]

emptysenv :: Senv
emptysenv = []

extendsenv :: String -> Senv -> Senv
extendsenv = (:)

applysenv :: Senv -> String -> Int
applysenv (s:ss) v = if s == v then 0 else (1 + (applysenv ss v))

initsenv :: Senv
initsenv = emptysenv
--                     --

translationofprog :: AST -> NLAST
translationofprog p = translationof p initsenv

translationof :: AST -> Senv -> NLAST
translationof (Const  x      ) s = NLConst x
translationof (Var    x      ) s = NLVar (applysenv s x)
translationof (Diff   e1 e2  ) s = NLDiff (translationof e1 s)
                                          (translationof e2 s)
translationof (Iszero e      ) s = NLIszero (translationof e s)
translationof (Ifte   c  t e ) s = NLIfte (translationof c s)
                                          (translationof t s)
                                          (translationof e s)
translationof (Let    v  e b ) s = NLLet (translationof e s)
                                         (translationof b (extendsenv v s))
translationof (ProcE  v  b   ) s = NLProc (translationof b (extendsenv v s))
translationof (CallE  f  a   ) s = NLCall (translationof f s)
                                          (translationof a s)
