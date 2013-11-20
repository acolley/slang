
import System.Environment

import Lexer
import Parser
import Eval

import Utils

-- extract
ext :: (Expr -> Either String Expr) -> Either String Expr -> Either String Expr
ext f (Left s) = Left s
ext f (Right expr) = f expr

-- main = putStrLn $ show $ lexer "(+ 20 20)"
--main = putStrLn $ show $ parseArgs [Num 10, Num 20, Num 30, Num 40, Sym "Poo", RParn]
--main = putStrLn $ show $ parseAdd [Num 10, Num 20, Num 30, Num 40, Num 50, RParn]
--main = putStrLn $ show $ (eval `ext` (parse $ lexer "(* (+ 1 2) 3 4)"))
--main = putStrLn $ show $ (eval `ext` (parse $ lexer "(- 10 20 (* 10 10))"))

parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn "No arguments given"
parseArgs (arg:args) =
    case lexer arg of
        Ok toks -> case eval `ext` (parse toks) of
                       Right e -> putStrLn $ show e
                       Left err -> putStrLn ("Error: " ++ err)
        Err s -> putStrLn ("Error: " ++ s)

main = do
    args <- getArgs
    parseArgs args
