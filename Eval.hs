module Eval where

import Data.Sort (sort)
import Data.List (union,transpose)
import Syntax
import Text.PrettyPrint.Boxes

getVars :: Expr -> [Ident]
getVars e = sort $ f e
    where 
      f e = case e of
        Var x         -> [x]
        Not e         -> f e
        And e1 e2     -> f e1 `union` f e2
        Or e1 e2      -> f e1 `union` f e2
        Implies e1 e2 -> f e1 `union` f e2
        Iff e1 e2     -> f e1 `union` f e2
        _             -> []

eval :: Expr -> Bool
eval e = 
  case e of
    BoolLit b      -> b
    Not e          -> not $ eval e
    And e1 e2      -> (eval e1) && (eval e2)
    Or  e1 e2      -> (eval e1) || (eval e2)
    Implies  e1 e2 -> (not (eval e1)) || (eval e2)
    Iff e1 e2      -> (eval e1) == (eval e2)
    _ -> error "unexpected construct"  

subst :: [(Ident,Bool)] -> Expr -> Expr
subst ls e = foldr (\x expr -> f x expr) e ls
    where f (var,b) expr = 
            case expr of 
              Var x -> if x == var then BoolLit b else Var x
              Not e         -> Not     (f (var,b) e)
              And e1 e2     -> And     (f (var,b) e1) (f (var,b) e2)
              Or e1 e2      -> Or      (f (var,b) e1) (f (var,b) e2)
              Implies e1 e2 -> Implies (f (var,b) e1) (f (var,b) e2)
              Iff e1 e2     -> Iff     (f (var,b) e1) (f (var,b) e2)
              _             -> expr

powerset :: Expr -> [[(Ident,Bool)]]
powerset e = f $ getVars e
    where
        f []     = []
        f [x]    = [[(x,True)],[(x,False)]]
        f (x:xs) = map ((x,True):)  (f xs)
                 ++ map ((x,False):) (f xs)


truthTable :: Expr -> ([[(Ident,Bool)]],[Bool])
truthTable e = (inputs,outputs)
    where
        inputs  = [(subset)                | subset <- powerset e]
        outputs = [(eval $ subst subset e) | subset <- powerset e]

check :: Expr -> Expr -> String
check e1 e2 = 
    if null $ filter (==True) (map (\(a,b) -> a && b) cs)
        then if null $ filter (==False) (map (\(a,b) -> a || b) cs)
            then "Inconsistent, Contradictory"
            else "Inconsistent, Contrary"
        else "Consistent"
    where (_,as) = truthTable e1
          (_,bs) = truthTable e2
          cs = zip as bs


formatTable :: Expr -> ([[(Ident,Bool)]],[Bool]) -> Box
formatTable e (inp,out) = hsep 2 left [(hsep 1 left cols),vcat left (text (show e):res)]
    where cols = map (vcat left) inputs
          inputs = transpose $ labels :(map.map) (\(_,b) -> cv b) inp
          labels = map (\(l,_) -> text l) (head inp)
          --dashes = replicate ((length.head) inp) (text "-")
          res = map cv out
          cv True  = text "T"
          cv False = text "F"

toLatex :: Expr -> ([[(Ident,Bool)]],[Bool]) -> String
toLatex expr (inp,out) = 
    "\\begin{array}" 
 ++ "{" ++ (replicate (length $ head inp) 'c') ++ "|c" ++ "}\n"
 ++ header ++ "\\\\\n"
 ++ "\\hline\n"
 ++ (foldr (\x y -> x ++ "\\\\\n" ++ y) "" body)
 ++ "\\end{array}"
    where cv True  = "T"
          cv False = "F"
          header   = foldr1 (\x y -> x ++ " & " ++ y) names ++ " & " ++ (showLatex expr)
          names    = map (\(a,b) -> a) (head inp)
          body     = map (\(a,b) -> (foldr1 (\x y -> x ++ " & " ++ y) a) ++ " & " ++ b) bools
          bools    = zip ((map.map) (\(a,b) -> cv b) inp) (map (\x -> cv x) out)
