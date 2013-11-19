
import Lexer
import Parser
import Eval

-- extract
ext :: (Expr -> Either String Expr) -> Either String Expr -> Either String Expr
ext f (Left s) = Left s
ext f (Right expr) = f expr

-- main = putStrLn $ show $ lexer "(+ 20 20)"
main = putStrLn $ show $ parse $ lexer "(* 20 20)"
