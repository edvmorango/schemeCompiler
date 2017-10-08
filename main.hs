module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char
import Data.Ratio
import Data.Complex
import Control.Monad(liftM)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             | Float Float
             | Rational Rational
             | Complex (Complex Float)
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

parseNumber :: Parser LispVal
parseNumber =    try parseComplex <|> try readFloat <|> try readRational <|> try readNumber <|> try parseBaseNumber

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
        char '#'
        s <- oneOf['t','f']
        return $ case s of
                't' -> Bool True
                'f' -> Bool False

parseChar :: Parser LispVal
parseChar = do
        string "#\\"
        c <- many1 letter
        return $ case c of
                "space" -> Char ' '
                "newline" -> Char '\n'
                [x] -> Char x

parseBaseNumber :: Parser LispVal
parseBaseNumber =  char '#' >>
        ((char 'd' >> parseNumber )
        <|> (char 'o' >> readOctalNumber)
        <|> (char 'x' >> readHexNumber))

parseComplex :: Parser LispVal
parseComplex =  do
          real <- fmap toDouble $  (try readFloat) <|> readNumber
          sign <- char '+' <|> char '-'
          imaginary <- fmap toDouble $  (try readFloat) <|> readNumber
          char 'i'
          let sImaginary = case sign of
                                  '+' -> imaginary
                                  '-' -> negate imaginary
          return $ Complex (real :+ sImaginary )
          where toDouble (Float x) = x
                toDouble (Number x) = fromInteger x :: Float

readFloat :: Parser LispVal
readFloat =  do
        whole <- many digit
        char '.'
        decimal <- many digit
        return $ Float (read (whole ++ "." ++ decimal) :: Float )

readRational :: Parser LispVal
readRational = do
        num <- many digit
        char '/'
        den <- many digit
        return $ Rational (read (num ++ "%" ++ den) :: Rational )

readNumber :: Parser LispVal
readNumber = (many1 digit) >>= (\n -> return ((Number . read) n))

readOctalNumber = readNumberInBase "01" 2
readHexNumber = readNumberInBase "0123456789abcdefABCDEF" 16

readNumberInBase :: String -> Integer -> Parser LispVal
readNumberInBase digits base = do
                    d <- many $ oneOf digits
                    return $ Number $ toDecimal base d

toDecimal :: Integer -> String -> Integer
toDecimal base s = foldl1 ((+) . (* base)) $ map toNumber s
                        where toNumber = (toInteger . digitToInt)

parseString :: Parser LispVal
parseString = do
        char '"'
        string <- many (escapedChars  <|> noneOf ['\\', '"'])
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
                
