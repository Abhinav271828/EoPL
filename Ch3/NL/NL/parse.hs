module Parse where

import Data.Char

-- Expression Datatype --
data AST = Const Int          |
           Var String         |
           Diff AST AST       |
           Iszero AST         |
           Let String AST AST |
           Ifte AST AST AST   |
           ProcE String AST   |
           CallE AST AST
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
              ("iszero":ls) -> (Iszero body, rest)
                                  where (body, rest) = parse ls
              ("if":ls) -> (Ifte c t e, rest)
                              where (c, "then":rem) = parse ls
                                    (t, "else":erem) = parse rem
                                    (e, rest) = parse erem
              ("λ":x:"->":ls) -> (ProcE x body, rest)
                                     where (body, rest) = parse ls
              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else case ls of
                                 "(":r -> (CallE (Var l) (arg), body)
                                            where (arg, ")":body) = parse r
                                 _ -> (Var l, ls)
