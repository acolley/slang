module Lexer ( Token(LParn,RParn,Num,Sym), lexer) where

import Data.Char

data Token =
    LParn
    | RParn
    | Num Int
    | Sym String
    deriving (Show)

isSymbolChar :: Char -> Bool
isSymbolChar c
    | isAlphaNum c || c `elem` "?-&*!$^#~;:+" = True
    | otherwise = False

lexer :: String -> [Token]
lexer [] = []
lexer ('(' : str) = LParn:lexer str
lexer (')' : str) = RParn:lexer str
lexer (c:str)
    | isDigit c = Num ((read (c : takeWhile isDigit str)) :: Int) : (lexer $ dropWhile isDigit str)
    | isAlpha c = Sym (c : takeWhile isSymbolChar str) : (lexer $ dropWhile isSymbolChar str)
    | isSpace c = lexer $ dropWhile isSpace str
    | otherwise = lexer str

main = putStrLn $ show $ lexer "(tits? 21)"
