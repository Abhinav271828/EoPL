module Parse where

import Data.Char

-- Expression Datatype --
data AST = Const  Int                     |
           Minus  Int                     |
           EmpL                           |
           Cons   AST             AST     |
           Car    AST                     |
           Cdr    AST                     |
           List   [AST]                   |
           Var    String                  |
           Bin    BOp             AST AST | 
           Iszero AST                     |
           IsEq   AST             AST     |
           IsLt   AST             AST     |
           IsGt   AST             AST     |
           Let    String          AST AST |
           Lets   [(String, AST)] AST     |
           Unpack [String]        AST AST |
           Ifte   AST             AST AST |
           Cond   [(AST, AST)]
           deriving Show

data BOp = B (Int -> Int -> Int)
instance Show BOp where
  show (B b) = show (b 1 1)
--                     --

scanparse :: String -> AST
scanparse prog = let (p, []) = parse (words prog)
                  in p

parse :: [String] -> (AST, [String])
parse lex = case lex of
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parse ls
                                           (body, rest) = parse rem
              ("-":"(":ls) -> (Bin (B (-)) op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("+":"(":ls) -> (Bin (B (+)) op1 op2, rest)
                                 where (op1, ",":rem) = parse ls
                                       (op2, ")":rest) = parse rem
              ("iszero":ls) -> (Iszero body, rest)
                                  where (body, rest) = parse ls
              ("car":ls) -> (Car body, rest)
                                  where (body, rest) = parse ls
              ("cdr":ls) -> (Cdr body, rest)
                                  where (body, rest) = parse ls
              ("-":x:ls) -> (Minus (read x :: Int), ls)
              ("if":ls) -> (Ifte c t e, rest)
                              where (c, "then":rem) = parse ls
                                    (t, "else":erem) = parse rem
                                    (e, rest) = parse erem
              ("[]":ls) -> (EmpL, ls)
              ("cons":ls) -> (Cons car cdr, rest)
                                where (car, rem) = parse ls
                                      (cdr, rest) = parse rem
              ("==":ls) -> (IsEq op1 op2, rest)
                                where (op1, rem) = parse ls
                                      (op2, rest) = parse rem
              ("<":ls) -> (IsLt op1 op2, rest)
                                where (op1, rem) = parse ls
                                      (op2, rest) = parse rem
              (">":ls) -> (IsGt op1 op2, rest)
                                where (op1, rem) = parse ls
                                      (op2, rest) = parse rem
              ("lets":ls) -> (Lets assm body, rest)
                                where (assm,"in":rem) = parselets ls
                                      (body, rest) = parse rem
                                      parselets (x:"=":xs) = ((x,val):assms , s)
                                                              where (val, r) = parse xs
                                                                    (assms,s) = case r of 
                                                                          ",":rs -> parselets rs
                                                                          "in":rs -> ([],r)
              ("list":"(":ls) -> (List es, rest)
                                where (es, rest) = parselist ls
                                      parselist exps = (e:r, rem)
                                                        where (e, s) = parse exps
                                                              (r, rem) = case s of
                                                                         ",":rs -> parselist rs
                                                                         ")":rs -> ([],rs)
              ("unpack":"(":ls) -> (Unpack vars val body, rest)
                                   where (vars, "=":rem) = parsestrs ls
                                         (val, "in":r) = parse rem
                                         (body, rest) = parse r
                                         parsestrs vs = (v:xv, s)
                                                        where (v:xs) = vs
                                                              (xv,s) = case xs of
                                                                        ",":rs -> parsestrs rs
                                                                        ")":rs -> ([],rs)
              ("cond":"(":ls) -> (Cond cs, rest)
                              where (cs,rest) = parseconds ls
                                    parseconds conds = ((c,t):rs, rem)
                                                        where (c, "->":r) = parse conds
                                                              (t, s) = parse r
                                                              (rs, rem) = case s of
                                                                           ",":xs -> parseconds xs
                                                                           ")":xs -> ([], xs)

              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else (Var l, ls)
