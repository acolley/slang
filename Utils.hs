module Utils (Result(Ok, Err), splitOn) where

import Data.List

data Result a = Ok a | Err String deriving (Eq, Show)

instance Functor Result where
    fmap f (Ok a) = Ok (f a)
    fmap f (Err s) = Err s

splitOn :: Char -> String -> Maybe (String, String)
splitOn c str =
    case findIndex (\x -> x == c) str of
        Just i -> Just (splitAt i str)
        Nothing -> Nothing

