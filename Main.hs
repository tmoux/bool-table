module Main where

import System.IO
import Parser
import Syntax
import Eval
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.Boxes

main = do
    putStr "> "
    hFlush stdout
    inp <- fmap (parse decl "") getLine   
    case inp of
      Left err -> do
        putStrLn $ "error: " ++ (show err)
        main
      Right Quit -> do
        putStrLn "Quitting."
      Right (Table e) -> do
        printBox $ formatTable e (truthTable e)
        main
      Right (Check e1 e2) -> do
        putStrLn $ check e1 e2
        main
      Right (Latex e) -> do
        putStrLn $ toLatex e (truthTable e)
        main
