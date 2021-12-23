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
           Isntzero AST                         |
           Let String AST AST                   |
           Letrec [(String,String,AST)] AST     |
           Ifte AST AST AST                     |
           ProcE String AST                     |
           Subr String Stmt                     |
           CallE AST AST
           deriving Show

-- Statement Datatype --
data Stmt = Eq String AST      |
            Print AST          |
            Read String        |
            Block [Stmt]       |
            Init String AST    |
            Cond AST Stmt Stmt |
            While AST Stmt     |
            DoWhile AST Stmt   |
            Decl [String] Stmt |
            CallS AST AST
            deriving Show

-- Program Datatype --
data Prog = Prog Stmt
            deriving Show

scanparse :: String -> Prog
scanparse prog = let (p, []) = parseProg (words prog)
                 in p

parseProg :: [String] -> (Prog, [String])
parseProg = (\(a,b) -> (Prog a, b)) . parseStmt

parseStmt :: [String] -> (Stmt, [String])
parseStmt lex = case lex of
                 (v:"=":ls) -> (Eq v val, rest)
                                 where (val, rest) = parseExp ls
                 ("print":ls) -> (Print exp, rest)
                                    where (exp, rest) = parseExp ls
                 ("read":x:ls) -> (Read x, ls)
                 ("{":ls) -> (Block (st:sts), rest)
                                where (st, rem) = parseStmt ls
                                      (Block sts, rest) = case rem of
                                                      ("}":r) -> (Block [], r)
                                                      (";":r) -> parseStmt ("{":r)
                 ("if":ls) -> (Cond c t e, rest)
                                where (c, rem) = parseExp ls
                                      (t, r) = parseStmt rem
                                      (e, rest) = parseStmt r
                 ("while":ls) -> (While exp body, rest)
                                    where (exp, rem) = parseExp ls
                                          (body, rest) = parseStmt rem
                 ("do-while":ls) -> (DoWhile exp body, rest)
                                      where (exp, rem) = parseExp ls
                                            (body, rest) = parseStmt rem
                 ("var":v:"=":ls) -> (Init v val, rest)
                                        where (val,rest) = parseExp ls
                 ("var":ls) -> (Decl ids body, rest)
                                 where ids = filter (/= ",") $ takeWhile (/= ";") ls
                                       (body, rest) = parseStmt $
                                                       tail $
                                                        dropWhile (/= ";") ls
                 (l:"(":ls) -> (CallS (Var l) arg, rest)
                                 where (arg, ")":rest) = parseExp ls

parseExp :: [String] -> (AST, [String])
parseExp lex = case lex of
              ("begin":ls) -> (Seq qs, rest)
                                 where (qs, rest) = parsexps ls
                                       parsexps xs = (p:ps, rs)
                                                        where (p, rem) = parseExp xs
                                                              (ps,rs) = case rem of
                                                                           ";":rst -> parsexps rst
                                                                           "end":rst -> ([], rst)
              ("set":x:"=":ls) -> (Assign x exp, rest)
                                        where (exp, rest) = parseExp ls
              ("let":v:"=":ls) -> (Let v (assm) (body), rest)
                                     where (assm, "in":rem) = parseExp ls
                                           (body, rest) = parseExp rem
              ("-":"(":ls) -> (Diff op1 op2, rest)
                                 where (op1, ",":rem) = parseExp ls
                                       (op2, ")":rest) = parseExp rem
              ("[]":ls) -> (EmpL, ls)
              ("cons":ls) -> (Cons car cdr, rest)
                                where (car, rem) = parseExp ls
                                      (cdr, rest) = parseExp rem
              ("car":ls) -> (Car body, rest)
                                  where (body, rest) = parseExp ls
              ("cdr":ls) -> (Cdr body, rest)
                                  where (body, rest) = parseExp ls
              ("iszero":ls) -> (Iszero body, rest)
                                  where (body, rest) = parseExp ls
              ("isntzero":ls) -> (Isntzero body, rest)
                                  where (body, rest) = parseExp ls
              ("if":ls) -> (Ifte c t e, rest)
                              where (c, "then":rem) = parseExp ls
                                    (t, "else":erem) = parseExp rem
                                    (e, rest) = parseExp erem
              ("λ":x:"->":ls) -> (ProcE x body, rest)
                               where (body, rest) = parseExp ls
              ("μ":x:"->":ls) -> (Subr x body, rest)
                               where (body, rest) = parseStmt ls
              ("letrec":ls) -> (Letrec funs body, rest)
                               where (funs, "in":rem) = parsefuns ls
                                     (body, rest) = parseExp rem
                                     parsefuns fs = ((x,a,b):r, rs)
                                                    where (x:"=":"λ":a:"->":s) = fs
                                                          (b, t) = parseExp s
                                                          (r, rs) = case t of
                                                                     ",":ts -> parsefuns ts
                                                                     "in":_ -> ([],t)
              (l:ls) -> if (all isDigit l)
                          then (Const (read l :: Int), ls)
                          else if (head l == 'r')
                                 then (RefE (read (tail l) :: Int), ls)
                                 else case ls of
                                   "(":r -> (CallE (Var l) (arg), body)
                                              where (arg, ")":body) = parseExp r
                                   _ -> (Var l, ls)
