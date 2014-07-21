module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad

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
	       rest <- many (letter <|> digit <|> symbol)
	       let atom = [first] ++ rest
	       return $ case atom of
			    "#t" -> Bool True
			    "#f" -> Bool False
			    otherwise -> Atom atom

-- parse numbers
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $many1 digit

	
-- parse lists (this is Lisp after all!)
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- parse the dotted-list
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
    
-- parse Scheme single-quote
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- parse a string | number | atom | etc
parseExpr :: Parser LispVal
parseExpr = parseAtom
	<|> parseString
	<|> parseNumber
	<|> parseQuoted
	<|> do char '('
	       x <- (try parseList) <|> parseDottedList
	       char ')'
	       return x
    
-- read in an expression			    
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val

-- Evaluator
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
    
main :: IO ()
main = do args <- getArgs
	  putStrLn (readExpr (args !! 0))
	    
	    