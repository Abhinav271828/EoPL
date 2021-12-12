module Parse where

import Data.Char

-- Expression Datatype --
data AST = Seq [AST]                            |
           Assign String AST                    |
           RefE Int                             |
           Const Int                            |
           Cons AST AST | EmpL                  |
           Car AST | Cdr AST                    |
           Var String                           |
           Diff AST AST                         |
           Iszero AST                           |
           Let String AST AST                   |
           LetM String AST AST                  |
           SetD String AST AST                  |
           Letrec [(String,[String],AST)] AST   |
           Ifte AST AST AST                     |
           ProcE [String] AST                   |
           CallE AST [AST]
           deriving Show

scanparse :: String -> AST
scanparse prog = let (p, []) = parse (words prog)
                 in p

parse :: [String] -> (AST, [String])
parse lex = case lex of
              ("begin":ls) -> (Seq qs, rest)
                                 where (qs, rest) = parsexps ls
                                       parsexps xs = (p:ps, rs)
                                                        where (p, rem) = parse xs
                                                              (ps,rs) = case rem of
                                                                           ";":rst -> parsexps rst
                                                                           "end":rst -> ([], rst)
              ("set":x:"=":ls) -> (Assign x exp, rest)
                                        where (exp, rest) = parse ls
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
                                           (body, rest) = parse rem
              ("letmutable":v:"=":ls) -> (LetM v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
                                           (body, rest) = parse rem
              ("setdynamic":v:"=":ls) -> (SetD v (assm) (body), rest)
                                     where (assm, "during":rem) = parse ls
                                           (body, rest) = parse rem
              ("-":"(":ls) -> (Diff op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("[]":ls) -> (EmpL, ls)
              ("cons":ls) -> (Cons car cdr, rest)
                                where (car, rem) = parse ls
                                      (cdr, rest) = parse rem
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
                          else if (head l == 'r')
                                 then (RefE (read (tail l) :: Int), ls)
                                 else case ls of
                                   "(":r -> (CallE (Var l) (args), rest)
                                            where (args, ")":rest) = parseargs r
                                                  parseargs as = (a:r, s)
                                                                    where (a,x) = parse as
                                                                          (r,s) = case x of
                                                                                   ",":rs -> parseargs rs
                                                                                   ")":_ -> ([],x)
                                   _ -> (Var l, ls)

