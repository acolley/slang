-- S ::= expr
-- expr ::= atom | "(" expr ")"
-- atom ::= number | symbol
-- number ::= [0-9]+
-- symbol ::= [A-Za-z_-][A-Za-z_-0-9]+

-- take in a [Token] and produce an AST, which should
-- be syntactically correct, otherwise produce an error

module Parser (
    Expr(Unit, Number, Boolean, Pair, Fst, Snd, Add, Mul, If, Var, Let, Fun, Closure, Call, IsUnit, Gt, Lt, Eq),
    Env, 
    parse) 
where

import Lexer

data Expr = 
    Unit
    | Number Int
    | Boolean Bool
    | Pair Expr Expr
    | Fst Expr
    | Snd Expr
    | Add Expr Expr
    | Mul Expr Expr
    | If Expr Expr Expr
    | Var String
    | Let String Expr Expr -- bind the result of first expr to the given string
    | Fun String String Expr -- empty string for name means anonymous Fun
    | Closure Env String String Expr -- holds an Env and a Fun
    | Call Expr Expr
    | IsUnit Expr
    | Gt Expr Expr
    | Lt Expr Expr
    | Eq Expr Expr
    deriving (Show)

type Env = [(String, Expr)]

peek :: [Token] -> Maybe Token
peek [] = Nothing
peek (tok:_) = Just tok

-- parsePair :: [Token] -> Either String (Expr, [Token])

-- parses a list of args, returning a cons Pair or Unit
-- for no arguments
--parseArgs :: [Token] -> Either String (Expr, [Token])
--parseArgs (RParn:toks) = Right (Unit, toks)
--parseArgs toks =
--    case parseExpr toks of
--        Right (e, toks1) -> Right (Pair e (parseArgs toks1))
--        Left s -> Left s

-- should produce an expression with nested pairs for the Mul expression
-- e.g. these are functionally the same (the first is sugar for the second):
-- (* 10 20)
-- (* (cons 10 (cons (20 null)))) (where null = Unit)
--parseMul :: [Token] -> Either String (Expr, [Token])
--parseMul toks =
--    case parseArgs toks of
--        Right (pair, rest) -> Right (Mul pair, rest)
--        Left s -> Left s

-- for multiple arguments in parseMul, second expression should be
-- a nested multiply with the other arguments?
-- e.g. (* 1 2 3 4) = (* 1 (* 2 (* 3 4))) (this appears to be how common Lisp does it)

-- take in an Expr to be applied to each pair of Tokens and
-- return an error String or the new nested Expr with the remaining Tokens
parseArgs :: Expr :: [Token] -> Either String (Expr, [Token])
parseArgs RParn:toks = 

parseMul :: [Token] -> Either String (Expr, [Token])
parseMul toks =
    case parseExpr toks of
        Right (e1, toks1) -> case parseExpr toks1 of
                                 Right (e2, toks2) -> Right (Mul e1 e2, toks2)
                                 Left s -> Left s
        Left s -> Left s

parseAdd :: [Token] -> Either String (Expr, [Token])
parseAdd toks =
    case parseExpr toks of
        Right (e1, toks1) -> case parseExpr toks1 of
                                 Right (e2, toks2) -> Right (Add e1 e2, toks2)
                                 Left s -> Left s
        Left s -> Left s

-- Since we don't know if the Symbol is a special keyword
-- or a user-defined binding we need to branch based on that
parseSymbol :: [Token] -> Either String (Expr, [Token])
parseSymbol [] = Left "Expected Symbol but received EOF"
parseSymbol (Sym s:toks) =
    case s of
        -- "cons" -- create a Pair
        -- "let"
        -- "if"
        -- "fst"
        -- "snd"
        "+" -> parseAdd toks -- consume '+' Symbol token, TODO: have this as a 'global' scope function?
        "*" -> parseMul toks -- consume '*' Symbol token
        _ -> Right (Var s, toks)
parseSymbol (tok:_) = Left ("Expected Symbol but received: " ++ (show tok))

parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr [] = Left "Unexpected EOF"
parseExpr (Num v:toks) = Right (Number v, toks)
parseExpr (Sym s:toks) = parseSymbol (Sym s:toks)
parseExpr (LParn:toks) =
    case peek toks of
        Just (Sym s) -> parseSymbol toks
        Just LParn -> parseExpr toks
        Just tok -> Left ("Expected LParn or Symbol but received: " ++ (show tok))
        Nothing -> Left "Unexpected EOF"
-- parseExpr RParn:toks =


parse :: [Token] -> Either String Expr
parse toks = case parseExpr toks of
                 Right (expr, _) -> Right expr
                 Left s -> Left s

--parse :: [Token] -> Either Expr
--parse toks = parseExpr toks
--parse [] = Left "Unexpected EOF"
--parse (Num v):toks = (Number v):parse toks
--parse (Sym s):toks = (Var s):parse toks
--parse LParn:toks = 
--    let next = peek toks
--    in case next of
--        (Sym s)

main = putStrLn $ show $ parseExpr [LParn, Sym "+", Num 10, Num 10, RParn]
