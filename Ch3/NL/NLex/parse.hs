module Parse where

import Data.Char

-- Expression Datatype --
data AST = Const Int          |
           Var String         |
           Diff AST AST       |
           Cons AST AST | EmpL |
           Car AST | Cdr AST |
           Iszero AST         |
           Lets [(String, AST)] AST |
           Letrec [(String, [String], AST)] AST |
           Ifte AST AST AST   |
           Cond [(AST, AST)]  |
           ProcE [String] AST   |
           CallE AST [AST]
           deriving Show

scanparse :: String -> AST
scanparse prog = let (p, []) = parse (words prog)
                 in p

parse :: [String] -> (AST, [String])
parse lex = case lex of
              ("lets":ls) -> (Lets assm body, rest)
                                     where (assm, "in":rem) = parselets ls
                                           (body, rest) = parse rem
                                           parselets (x:"=":xs) = ((x,val):assms, s)
                                                                where (val,r) = parse xs
                                                                      (assms,s) = case r of
                                                                                  ",":rs -> parselets rs
                                                                                  "in":rs -> ([],r)
              ("-":"(":ls) -> (Diff op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("cons":"(":ls) -> (Cons op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("[]":ls) -> (EmpL, ls)
              ("car":ls) -> (Car body, rest)
                                  where (body, rest) = parse ls
              ("cdr":ls) -> (Cdr body, rest)
                                  where (body, rest) = parse ls
              ("iszero":ls) -> (Iszero body, rest)
                                  where (body, rest) = parse ls
              ("if":ls) -> (Ifte c t e, rest)
                              where (c, "then":rem) = parse ls
                                    (t, "else":erem) = parse rem
                                    (e, rest) = parse erem
              ("cond":"(":ls) -> (Cond cs, rest)
                              where (cs,rest) = parseconds ls
                                    parseconds conds = ((c,t):rs, rem)
                                                        where (c, "->":r) = parse conds
                                                              (t, s) = parse r
                                                              (rs, rem) = case s of
                                                                           ",":xs -> parseconds xs
                                                                           ")":xs -> ([], xs)
              ("λ":ls) -> (ProcE args body, rest)
                                     where (args, "->":rem) = parseargs ls
                                           (body, rest) = parse rem
                                           parseargs as = (a:r, s)
                                                            where (a:x) = as
                                                                  (r,s) = case x of
                                                                        ",":rs -> parseargs rs
                                                                        "->":_ -> ([], x)
              ("letrec":ls) -> (Letrec funs body, rest)
                               where (funs, "in":rem) = parsefuns ls
                                     (body, rest) = parse rem
                                     parsefuns fs = ((x,as,b):r, rs)
                                                    where (x:"=":"λ":args) = fs
                                                          (as, "->":s) = parseargs args
                                                          (b, t) = parse s
                                                          (r, rs) = case t of
                                                                     ",":ts -> parsefuns ts
                                                                     "in":_ -> ([],t)
                                                          parseargs bs = (b:u, v)
                                                                         where (b:y) = bs
                                                                               (u,v) = case y of
                                                                                        ",":ys -> parseargs ys
                                                                                        "->":_ -> ([],y)
              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else case ls of
                                 "(":r -> (CallE (Var l) (args), body)
                                            where (args, ")":body) = parseargs r
                                                  parseargs as = (a:r, s)
                                                                    where (a,x) = parse as
                                                                          (r,s) = case x of
                                                                                  ",":rs -> parseargs rs
                                                                                  ")":_ -> ([],x)
                                 _ -> (Var l, ls)
