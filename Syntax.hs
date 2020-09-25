module Syntax where

type Ident = String

data Decl = Table Expr
          | Check Expr Expr
          | Latex Expr
          | Quit
    deriving (Show)

data Expr = Var Ident
          | BoolLit Bool
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Implies Expr Expr
          | Iff Expr Expr

instance Show Expr where
    show (Var x)         = x
    show (BoolLit b)     = show b
    show (Not e)         = "~(" ++ show e ++ ")"
    show (And e1 e2)     = "(" ++ show e1 ++ " & " ++ show e2 ++ ")"
    show (Or e1 e2)      = "(" ++ show e1 ++ " | " ++ show e2 ++ ")"
    show (Implies e1 e2) = "(" ++ show e1 ++ " -> " ++ show e2 ++ ")"
    show (Iff e1 e2)     = "(" ++ show e1 ++ " = " ++ show e2 ++ ")"

showLatex e = case e of
    Var x         -> x
    BoolLit b     -> show b
    Not e         -> "\\lnot (" ++ (showLatex e) ++ ")"
    And e1 e2     -> "(" ++ (showLatex e1) ++ " \\land " ++ (showLatex e2) ++ ")"
    Or e1 e2      -> "(" ++ (showLatex e1) ++ " \\lor " ++ (showLatex e2) ++ ")"
    Implies e1 e2 -> "(" ++ (showLatex e1) ++ " \\supset " ++ (showLatex e2) ++ ")"
    Iff e1 e2     -> "(" ++ (showLatex e1) ++ " \\equiv " ++ (showLatex e2) ++ ")"
