module Main where 

import Data.Char
import Control.Applicative

data RegExp
    = Epsilon
    | Unione RegExp RegExp
    | Star RegExp
    | Concat RegExp RegExp
    | Empty
    | Term Char
    deriving (Show, Eq)

newtype Parser a = Parser (String -> Maybe (String, a))

main :: IO ()
main = undefined