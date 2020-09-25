module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Syntax

languageDef =
    emptyDef { Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = [ "Table"
                                      , "Check"
                                      , "Latex"
                                      , "Quit"
                                      ]
            , Token.reservedOpNames = [ "~"
                                      , "&"
                                      , "|"
                                      , "->"
                                      , "="
                                      ]
    }
              
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
integer    = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

--parser definition
parseProgram :: Parser [Decl]
parseProgram = whiteSpace >> parseProgram'
    where parseProgram' = many1 (decl <* reservedOp ".")

decl :: Parser Decl
decl = ttDecl 
   <|> checkDecl
   <|> latexDecl
   <|> quitDecl

ttDecl = (\x -> Table x) <$ reserved "Table" <*> expr

latexDecl = (\x -> Latex x) <$ reserved "Latex" <*> expr

checkDecl = (\e1 e2 -> Check e1 e2) <$ reserved "Check" <*> expr <*> expr

quitDecl = Quit <$ reserved "Quit"

expr  = buildExpressionParser table term
        <?> "expression"

term  =  parens expr 
     <|> (\x -> Var x) <$>identifier
     <?> "primary expression"

table = [ [prefix "~" (\x -> Not x)]
        , [binary "&" (\x y -> And x y) AssocLeft]
        , [binary "|" (\x y -> Or x y) AssocLeft]
        , [binary "->" (\x y -> Implies x y) AssocRight]
        , [binary "=" (\x y -> Iff x y) AssocLeft]
        ]
        --only -> is non-associative
         
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })
