module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             | Float Float
             deriving (Eq, Show)  


main :: IO ()
main = do 
       args <- getArgs
       putStrLn (readExpr (args !! 0))


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
        Left err -> "No match" ++ show err
        Right val -> show val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber
        <|> try parseBool
        <|> try parseString
        <|> try parseChar
        
parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ Atom atom

parseString :: Parser LispVal
parseString = do
        char '"'
        string <- many (noneOf ['\\', '"'])
        char '"'
        return $ String string

escapedChars :: Parser Char
escapedChars = do
        char '\\'
        c <- oneOf['\\', 'n', 'r', 't']
        return $ case c of
                '\\' -> c
                '"' -> c
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'

parseChar :: Parser LispVal
parseChar = do
        string "#\\"
        c <- many1 letter
        return $ case c of
                "space" -> Char ' '
                "newline" -> Char '\n'
                [x] -> Char x

parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (\n -> return ((Number . read) n))

parseBool :: Parser LispVal
parseBool = do
        char '#'
        s <- oneOf['t','f'] 
        return $ case s of
                't' -> Bool True
                'f' -> Bool False
