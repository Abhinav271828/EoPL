module Interpret where

import Parse
import Data.Char

-- Procedure Datatype (Prelim) --
type Proc = ExpVal -> Store -> (ExpVal, Store)

proc :: String -> AST -> Env -> Proc
proc v b r = \e s -> let (i, s1) = (newref e s)
                     in valueofexp b (extendenv v (Refv i) r) s1

applyproc :: Proc -> ExpVal -> Store -> (ExpVal, Store)
applyproc = id
--                             --
-- Subroutine Datatype (Prelim) --
type Subr = ExpVal -> Store -> IO Store

subr :: String -> Stmt -> Env -> Subr
subr v b r = \e s -> let (i, s1) = newref e s
                     in valueofstmt b (extendenv v (Refv i) r) s1

applysubr :: Subr -> ExpVal -> Store -> IO Store
applysubr = id
--                              --

-- Expressed Values Datatype --
data ExpVal = Numv Int | Boolv Bool | Procv Proc | Listv [ExpVal] | Refv Int | Subrv Subr | Null

instance Show ExpVal where
  show (Numv  n) = "Numv " ++ show n
  show (Boolv b) = "Boolv " ++ show b
  show (Procv p) = "Procv " ++ show (p (Refv 0) [Numv 2])
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

deref :: Int -> Store -> ExpVal
deref i s = s !! i

setref :: Int -> ExpVal -> Store -> Store
setref i val s = (take i s) ++ (val : (drop (i+1) s))
--                --


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

run :: String -> IO ()
run p = do _ <- valueofprog p initenv emptystore
           return ()

valueofprog :: String -> Env -> Store -> IO Store
valueofprog p r s = valueofstmt stmt r s
                      where Prog stmt = scanparse p

valueofstmt :: Stmt -> Env -> Store -> IO Store
valueofstmt (Eq    var  val    ) r s = let (exp, s1) = valueofexp val r s
                                           Refv i = applyenv r var
                                       in return $ setref i exp s1
valueofstmt (Print exp         ) r s = let (val, s1) = valueofexp exp r s
                                       in do print val
                                             return s1
valueofstmt (Read  var         ) r s = do val' <- getLine
                                          let val = read val' :: Int
                                          let Refv i = applyenv r var
                                          return $ setref i (Numv val) s
{-
valueofstmt (Block stmts       ) r s = case stmts of
                                         [] -> return s
                                         ((Init v e):sts)
                                                  -> let (val, s1) = valueofexp e r s
                                                         (i, s2) = newref val s1
                                                     in valueofstmt (Block sts)
                                                              (extendenv v (Refv i) r) s2
                                         (st:sts) -> do s1 <- valueofstmt st r s
                                                        valueofstmt (Block sts) r s1 -}
valueofstmt (Block stmts       ) r s = let inits = takeWhile isInit stmts
                                           varInits = takeWhile isInitVar inits
                                           funInits = dropWhile isInitVar inits
                                           body  = dropWhile isInit stmts
                                           (r1,s1) = initVars varInits r s
                                           (r2,s2) = initFuns funInits r1 s1
                                       in case body of
                                            [] -> return s2
                                            (st:sts) -> do s3 <- valueofstmt st r2 s2
                                                           valueofstmt (Block sts) r2 s3
valueofstmt (Cond  cond t    e ) r s = let (Boolv c, s1) = valueofexp cond r s
                                       in if c then valueofstmt t r s1
                                          else valueofstmt e r s1
valueofstmt (While cond body   ) r s = let (Boolv c, s1) = valueofexp cond r s
                                       in if c then do s2 <- valueofstmt body r s1
                                                       valueofstmt (While cond body) r s2
                                          else return s1
valueofstmt (DoWhile cond body ) r s = do s1 <- valueofstmt body r s
                                          let (Boolv c, s2) = valueofexp cond r s
                                          if c then valueofstmt (While cond body) r s2
                                          else return s1
valueofstmt (Decl  vars body   ) r s = let (addrs, s1) = newrefs vars s
                                           r1 = foldr ($) r [extendenv var (Refv addr)
                                                               | (var, addr) <- zip vars addrs]
                                       in valueofstmt body r1 s1

valueofstmt (CallS rat  ran    ) r s = let (Subrv fun, s1) = valueofexp rat r s
                                           (arg, s2) = valueofexp ran r s1
                                       in applysubr fun arg s2

isInit :: Stmt -> Bool
isInit (Init _ _) = True
isInit _          = False

isInitVar :: Stmt -> Bool
isInitVar (Init _ (ProcE _ _)) = False
isInitVar (Init _ _          ) = True

initVars :: [Stmt] -> Env -> Store -> (Env,Store)
initVars []                   r s = (r,s)
initVars ((Init var exp):sts) r s = let (val, s1) = valueofexp exp r s
                                    in initVars sts (extendenv var val r) s1

initFuns :: [Stmt] -> Env -> Store -> (Env,Store)
initFuns inits r s = extendenvrec (map (\(Init name (ProcE var body)) -> (name,var,body)) inits) (r,s)

valueofexp :: AST -> Env -> Store -> (ExpVal, Store)
valueofexp (Const  x           ) r s = (Numv x, s)
valueofexp (RefE   i           ) r s = (Refv i, s)
valueofexp (Assign x   b       ) r s = let (exp, s1) = valueofexp b r s
                                           Refv i = applyenv r x
                                        in (Numv 42, setref i exp s1)
valueofexp (Cons   a   d       ) r s = let (v1, s1) = valueofexp a r s
                                           (Listv v2, s2) = valueofexp d r s1
                                       in (Listv (v1:v2) , s2)
valueofexp (Car    l           ) r s = let (Listv v, s1) = valueofexp l r s
                                       in (head v, s1)
valueofexp (Cdr    l           ) r s = let (Listv v, s1) = valueofexp l r s
                                       in (Listv (tail v), s1)
valueofexp (EmpL               ) r s = (Listv [], s)
valueofexp (Var    v           ) r s = let Refv i = applyenv r v
                                       in (deref i s, s)
valueofexp (Diff   e1  e2      ) r s = let (Numv v1, s1) = valueofexp e1 r s
                                           (Numv v2, s2) = valueofexp e2 r s1
                                       in (Numv (v1 - v2), s2)
valueofexp (Iszero e           ) r s = let (Numv v, s1) = valueofexp e r s
                                       in (Boolv (v == 0), s1)
valueofexp (Isntzero e         ) r s = let (Numv v, s1) = valueofexp e r s
                                       in (Boolv (v /= 0), s1)
valueofexp (Let    var e   b   ) r s = let (val, s1) = valueofexp e r s
                                           (i, s2) = newref val s1
                                       in valueofexp b (extendenv var (Refv i) r) s2
valueofexp (Letrec fns b       ) r s = let (r1, s1) = extendenvrec fns (r,s)
                                       in valueofexp b r1 s1
valueofexp (Ifte   c   t   e   ) r s = let (Boolv v, s1) = valueofexp c r s
                                       in if v
                                          then (valueofexp t r s1)
                                          else (valueofexp e r s1)
valueofexp (ProcE  v   b       ) r s = (Procv (proc v b r), s)
valueofexp (Subr   v   b       ) r s = (Subrv (subr v b r), s)
valueofexp (CallE  rat ran     ) r s = let (Procv fun, s1) = valueofexp rat r s
                                           (arg, s2) = valueofexp ran r s1
                                       in applyproc fun arg s2
valueofexp (Seq    exps        ) r s = let (v, s1) = valueofexp (head exps) r s
                                       in case (tail exps) of
                                        [] -> (v,s1)
                                        _  -> valueofexp (Seq (tail exps)) r s1
--                     --
