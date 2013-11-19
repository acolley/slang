module Lexer ( Token(LParn,RParn,Num,Sym), lexer ) where

import Data.Char

data Token =
    LParn
    | RParn
--    | Quote
    | Num Int
    | Sym String
    deriving (Show)

isSymbolStart :: Char -> Bool
isSymbolStart c = isAlpha c || c `elem` "?-&*/!$^#~;:+"

isSymbolChar :: Char -> Bool
isSymbolChar c = isSymbolStart c || isDigit c

lexer :: String -> [Token]
lexer [] = []
lexer ('(' : str) = LParn:lexer str
lexer (')' : str) = RParn:lexer str
-- lexer ('\'':str) = Quote:lexer str
lexer (c:str)
    | isDigit c = Num ((read (c : takeWhile isDigit str)) :: Int) : (lexer $ dropWhile isDigit str)
    | isSymbolStart c = Sym (c : takeWhile isSymbolChar str) : (lexer $ dropWhile isSymbolChar str)
    | isSpace c = lexer $ dropWhile isSpace str
    | otherwise = lexer str

main = putStrLn $ show $ lexer "(tits? 21)"
