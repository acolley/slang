module Lexer ( Token(LParn,RParn,Num,StrLit,Sym), lexer ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State.Lazy
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

--type Lexer a = StateT String (ErrorT String Identity) a
--type Lexer a = ReaderT String (ErrorT String Identity) a
--
--runLexer :: String -> Lexer a -> Either String a
--runLexer str l = runIdentity $ runErrorT $ runReaderT l str
--
--runLexer :: String -> Lexer a -> Either String (a, String)
--runLexer st l = runIdentity $ runErrorT $ runStateT l st

data LexerState = LexerState {
    lexLine :: Int,
    lexColumn :: Int
}

type Lexer a = StateT LexerState (ErrorT String Identity) a

-- note: this calls evalStateT instead of runStateT in order
-- to throw away the unneeded state at the end
-- to propagate line numbers and such through the system a Token
-- should also store this information against itself
runLexer :: LexerState -> Lexer a -> Either String a
runLexer st l = runIdentity $ runErrorT $ evalStateT l st

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
        Just (pre, post) -> return (StrLit pre, tail post)
        Nothing -> Err "Expected closing quote"

lexDigit :: String -> Result (Token, String)
lexDigit str = 
    case reads str of
        [(v, rest)] -> return (Num v, rest)
        _ -> Err "Could not parse Num"

lexSymbol :: String -> Result (Token, String)
lexSymbol str = let (sym, rest) = span isSymbolChar str
                in return (Sym sym, rest)

lexer :: String -> Result [Token]
lexer [] = return []
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

