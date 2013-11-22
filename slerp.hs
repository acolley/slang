
import System.Environment

import Lexer
import Parser
import Eval

import Utils

interpret :: String -> String
interpret str =
    case lexer str of
        Ok toks -> case parse toks of
                       Ok e -> case eval e of
                                   Ok oute -> show oute
                                   Err s -> "Error: " ++ s
                       Err s -> "Error: " ++ s
        Err s -> "Error: " ++ s

main = do
    args <- getArgs
    lines <- readFile $ head args
    putStrLn $ interpret $ lines
