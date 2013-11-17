
import Lexer
import Parser
import Eval

-- extract
ext :: (Expr -> Either String Expr) -> Either String Expr -> Either String Expr
ext f (Left s) = Left s
ext f (Right expr) = f expr

main = putStrLn $ show (ext eval (parse $ lexer "10"))