module Parse where

import Data.Char

-- Expression Datatype --
data AST = Seq [AST]                            |
           Assign String AST                    |
           RefE Int                             |
           Const Int                            |
           NewPair AST AST                      |
           LeftE AST | RightE AST               |
           SetLeft String AST                   |
           SetRight String AST                  |
           Var String                           |
           Diff AST AST                         |
           Iszero AST                           |
           Let String AST AST                   |
           Letrec [(String,String,AST)] AST     |
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
              ("set":x:"=":ls) -> (Assign x exp, rest)
                                        where (exp, rest) = parse ls
              ("setleft":x:"=":ls) -> (SetLeft x exp, rest)
                                        where (exp, rest) = parse ls
              ("setright":x:"=":ls) -> (SetRight x exp, rest)
                                        where (exp, rest) = parse ls
              ("newpair":"(":ls) -> (NewPair e1 e2, rest)
                                   where (e1, ",":rem) = parse ls
                                         (e2, ")":rest) = parse rem
              ("left":"(":ls) -> (LeftE e, rest)
                                   where (e, ")":rest) = parse ls
              ("right":"(":ls) -> (RightE e, rest)
                                   where (e, ")":rest) = parse ls
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
                                           (body, rest) = parse rem
              ("-":"(":ls) -> (Diff op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
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
