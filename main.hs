
import Lexer
import Parser
import Eval

-- extract
ext :: (Expr -> Either String Expr) -> Either String Expr -> Either String Expr
ext f (Left s) = Left s
ext f (Right expr) = f expr

-- main = putStrLn $ show $ lexer "(+ 20 20)"
--main = putStrLn $ show $ parseArgs [Num 10, Num 20, Num 30, Num 40, Sym "Poo", RParn]
--main = putStrLn $ show $ parseAdd [Num 10, Num 20, Num 30, Num 40, Num 50, RParn]
--main = putStrLn $ show $ (eval `ext` (parse $ lexer "(* (+ 1 2) 3 4)"))
main = putStrLn $ show $ (eval `ext` (parse $ lexer "(- 10 20 (* 10 10))"))