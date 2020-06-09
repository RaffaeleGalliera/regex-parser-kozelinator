module Main where

import Data.Char
import Control.Applicative
import System.Environment (getArgs)

data RegExp
    = Epsilon
    | Term Char
    | Star RegExp
    | Concatena RegExp RegExp
    | Unione RegExp RegExp
    deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input' , f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys) | y == x    = Just (ys, x)
               | otherwise = Nothing
    f [] = Nothing

term :: Parser RegExp
term = Term <$> foldr ((<|>) . charP) empty (['a' .. 'z'] ++ ['0' .. '9'])

epsilon :: Parser RegExp
epsilon = (\_ -> Epsilon) <$> charP '&'

subRegExp :: Parser RegExp
subRegExp = charP '(' *> regExp <* charP ')'

factor :: Parser RegExp
factor = term <|> subRegExp <|> epsilon

star :: Parser RegExp
star = Star <$> (factor <* charP '*')

concatena :: Parser RegExp
concatena =
    Concatena
        <$> (star <|> factor)
        <*> (concatena <|> star <|> factor)

unione :: Parser RegExp
unione =
    Unione
        <$> ((concatena <|> star <|> factor) <* charP '+')
        <*> (regExp <|> subRegExp)

regExp :: Parser RegExp
regExp = unione <|> concatena <|> star <|> factor

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

printFormatted :: (String, Maybe RegExp) -> IO ()
printFormatted (stringRegExp, Just x) = putStrLn $ stringRegExp ++ ": " ++ show x

main :: IO ()
main = do
    args <- getArgs 
    content <- readLines (head args)
    let parsed = map (fmap snd . runParser regExp) content
    mapM_ printFormatted $ zip content parsed
    -- mapM_ is equivalent to sequence_ $ fmap ...

