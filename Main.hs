module Main where 

import Data.Char
import Control.Applicative

data RegExp
    = Epsilon
    | Unione RegExp RegExp
    | Star RegExp
    | Concatena RegExp RegExp
    | Empty
    | Term Char
    deriving (Show, Eq)

newtype Parser a = Parser 
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
        (input', x) <- p input
        Just (input', f x)
    
instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f[] = Nothing
        
term :: Parser RegExp
term = Term <$> foldr ((<|>) . charP) empty (['a'..'z'] ++ ['0'..'9'])

star :: Parser RegExp
star = Star <$> ((term <|> subRegExp) <* charP '*')

concatena :: Parser RegExp
concatena = Concatena <$> (star <|> term <|> subRegExp) <*> (concatena <|> star <|> term <|> subRegExp)

unione :: Parser RegExp
unione = Unione <$> ((concatena <|> star <|> term <|> subRegExp) <* charP '+') <*> (regExp <|> subRegExp) 

subRegExp :: Parser RegExp
subRegExp = charP '(' *> regExp <* charP ')'

regExp :: Parser RegExp
regExp = unione <|> concatena <|> star <|> term <|> subRegExp

main :: IO ()
main = undefined

