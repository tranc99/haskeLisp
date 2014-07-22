module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import Control.Monad.Error

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
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

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

-- evaluate numbers, strings, booleans etc
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- apply
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

--primitives support
primitives :: [ (String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


-- data type to represent an error
data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParserError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String
    
-- print out the Scheme error types
showError :: LispError -> String
showError (Unboundvar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show expected
                                    ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ ", found " ++ show found
    
main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

	    
	    