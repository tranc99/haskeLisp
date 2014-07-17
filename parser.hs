module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- parse Lisp symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- parse 

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val 
    
main :: IO ()
main = do args <- getArgs
	  putStrLn (readExpr (args !! 0))
	    
	    