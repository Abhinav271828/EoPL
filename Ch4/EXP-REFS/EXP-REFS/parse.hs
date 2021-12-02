module Parse where

import Data.Char

-- Expression Datatype --
data AST = Seq [AST]                            |
           Newref AST | Deref AST               |
           Setref AST AST                       |
           RefE Int                             |
           Const Int                            |
           Cons AST AST | EmpL                  |
           Car AST | Cdr AST                    |
           Var String                           |
           Diff AST AST                         |
           Iszero AST                           |
           Let String AST AST                   |
           Letrec [(String, String, AST)] AST   |
           Ifte AST AST AST                     |
           ProcE String AST                     |
           CallE AST AST
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
              ("newref":"(":ls) -> (Newref v, rest)
                                 where (v, ")":rest) = parse ls
              ("deref":"(":ls) -> (Deref v, rest)
                                 where (v, ")":rest) = parse ls
              ("setref":"(":ls) -> (Setref ref exp, rest)
                                        where (ref, ",":rem) = parse ls
                                              (exp, ")":rest) = parse rem
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
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
              ("λ":x:"->":ls) -> (ProcE x body, rest)
                               where (body, rest) = parse ls
              ("letrec":ls) -> (Letrec funs body, rest)
                               where (funs, "in":rem) = parsefuns ls
                                     (body, rest) = parse rem
                                     parsefuns fs = ((x,a,b):r, rs)
                                                    where (x:"=":"λ":a:"->":s) = fs
                                                          (b, t) = parse s
                                                          (r, rs) = case t of
                                                                     ",":ts -> parsefuns ts
                                                                     "in":_ -> ([],t)
              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else if (head l == 'r')
                                 then (RefE (read (tail l) :: Int), ls)
                                 else case ls of
                                   "(":r -> (CallE (Var l) (arg), body)
                                              where (arg, ")":body) = parse r
                                   _ -> (Var l, ls)
