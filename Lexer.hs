module Lexer ( Token(LParn,RParn,Num,StrLit,Sym), lexer ) where

import Control.Applicative
import Data.Char
import Data.List

import Utils

data Token =
    LParn
    | RParn
--    | Quote
    | Num Int
    | StrLit String
    | Sym String
    deriving (Eq, Show)

isComment :: Char -> Bool
isComment c = c == ';'

isEOL :: Char -> Bool
isEOL c = c `elem` "\n\r"

isSymbolStart :: Char -> Bool
isSymbolStart c = isAlpha c || c `elem` "=<>?-&*/!$^#~:+"

isSymbolChar :: Char -> Bool
isSymbolChar c = isSymbolStart c || isDigit c

lexStr :: String -> Result (Token, String)
lexStr str = 
    case splitOn '"' str of
        Just (pre, post) -> Ok (StrLit pre, (tail post))
        Nothing -> Err "Expected closing quote"

lexDigit :: String -> Result (Token, String)
lexDigit str = Ok (Num ((read (takeWhile isDigit str)) :: Int), dropWhile isDigit str)

lexSymbol :: String -> Result (Token, String)
lexSymbol str = Ok (Sym (takeWhile isSymbolChar str), dropWhile isSymbolChar str)

lexer :: String -> Result [Token]
lexer [] = Ok []
lexer ('(' : str) = (LParn:) <$> lexer str
lexer (')' : str) = (RParn:) <$> lexer str
-- lexer ('\'':str) = Quote:lexer str
lexer (c:str)
    | isDigit c = do
        (num, rest) <- lexDigit (c:str)
        (num:) <$> lexer rest
    | c == '"' = do
        (tok, rest) <- lexStr str
        (tok:) <$> lexer rest
    | isSymbolStart c = do
        (sym, rest) <- lexSymbol (c:str)
        (sym:) <$> lexer rest
    | isComment c = lexer $ dropWhile (\x -> not (isEOL x)) str
    | isSpace c = lexer $ dropWhile isSpace str
    | otherwise = lexer str

