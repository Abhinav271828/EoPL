module Parse where

import Data.Char

-- Expression Datatype --
data AST = Const Int          |
           Var String         |
           Diff AST AST       |
           Mult AST AST       |
           Iszero AST         |
           Let String AST AST |
           Ifte AST AST AST   |
           ProcE [String] AST |
           CallE AST [AST]
           deriving Show

scanparse :: String -> AST
scanparse prog = let (p, []) = parse (words prog)
                 in p

parse :: [String] -> (AST, [String])
parse lex = case lex of
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
                                           (body, rest) = parse rem
              ("-":"(":ls) -> (Diff op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("*":"(":ls) -> (Mult op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
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
              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else case ls of
                                 "(":r -> (CallE (Var l) (args), rest)
                                            where (args, ")":rest) = parseargs r
                                                  parseargs as = (a:r, s)
                                                                    where (a,x) = parse as
                                                                          (r,s) = case x of
                                                                                   ",":rs -> parseargs rs
                                                                                   ")":_ -> ([],x)
                                 _ -> (Var l, ls)
