module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- parse Lisp symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- parse white-space
spaces :: Parser ()
spaces = skipMany1 space

-- data type to hold Lisp values
data LispVal = Atom String
	      | List [LispVal]
	      | DottedList [LispVal] LispVal
	      | Number Integer
	      | String String
	      | Bool Bool

-- strings	      
parseString :: Parser LispVal
parseString = do char '"'
		 x <- many (noneOf "\"")
		 char '"'
		 return $ String x

-- parse Scheme variables
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
	       rest <- (letter <|> digit <|> symbol)
	       let atom = [first] ++ rest
	       return $ case atom of
			    "#t" -> Bool True
			    "#f" -> Bool False
			    otherwise -> Atom atom

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val 
    
main :: IO ()
main = do args <- getArgs
	  putStrLn (readExpr (args !! 0))
	    
	    