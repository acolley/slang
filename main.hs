
import System.Environment

import Lexer
import Parser
import Eval

import Utils

-- extract
ext :: (Expr -> Result Expr) -> Result Expr -> Result Expr
ext f (Err s) = Err s
ext f (Ok expr) = f expr

-- main = putStrLn $ show $ lexer "(+ 20 20)"
--main = putStrLn $ show $ parseArgs [Num 10, Num 20, Num 30, Num 40, Sym "Poo", RParn]
--main = putStrLn $ show $ parseAdd [Num 10, Num 20, Num 30, Num 40, Num 50, RParn]
--main = putStrLn $ show $ (eval `ext` (parse $ lexer "(* (+ 1 2) 3 4)"))
--main = putStrLn $ show $ (eval `ext` (parse $ lexer "(- 10 20 (* 10 10))"))
--main = putStrLn $ show $ eval (Call (Var "snd") (Pair (Number 10) Unit)) -- test global funcs "fst" and "snd"
--main = putStrLn $ show $ eval (Call (Call (Var "cons") (Number 10)) (Number 20))
--main = putStrLn $ show $ parseSyntax [LParn, LParn, Num 10, RParn, RParn]
--main = putStrLn $ show $ eval (Call (Fun "" ["x", "y"] (Add (Var "x") (Var "y"))) [Number 10, Number 20]) -- test multi argument functions
--main = putStrLn $ show $ eval (Call (Var "+") [Number 1, Number 1])
--main = putStrLn $ show $ hlist_to_slist [Number 10, Number 20, Number 30]
--main = putStrLn $ show $ slist_to_hlist (hlist_to_slist [Number 10, Number 20, Number 30])
--main = putStrLn $ show $ eval (Call (Var "nil?") [(Unit)])
--main = putStrLn $ show $ eval (Call (Var "list") [Number 1, Number 2, Number 3])
--main = putStrLn $ show $ eval (Call (Var "pair?") [Pair (Number 1) (Number 2)])
--main = putStrLn $ show $ eval (Call (Var "=") [Number 1, Number 1])
--main = putStrLn $ show $ eval (Call (Var "list?") [(hlist_to_slist [Number 1, Number 2, Number 3])])
main = putStrLn $ show $ eval (Call (Var "map") [(Fun "" [ArgNamed "x"] (Add (Var "x") (Number 1))), (hlist_to_slist [Number 1, Number 2, Number 3])])


parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn "No arguments given"
parseArgs (arg:args) =
    case lexer arg of
        Ok toks -> case eval `ext` (parse toks) of
                       Ok e -> putStrLn $ show e
                       Err s -> putStrLn ("Error: " ++ s)
        Err s -> putStrLn ("Error: " ++ s)

--main = do
--    args <- getArgs
--    putStrLn $ head args
--    parseArgs args
