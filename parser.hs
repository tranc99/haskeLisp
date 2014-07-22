module Main where
import Monad
import System.Environment
import Control.Monad.Error
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
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- apply
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

--primitives support
primitives :: [ (String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


-- data type to represent an error
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
    
-- print out the Scheme error types
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
        noMsg = Default "An error has occured"
        strMsg = Default

-- represent functions that may throw a LispError or return a value
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
    
main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0)
        putStrLn $ extractValue $ trapError evaled

	    
	    