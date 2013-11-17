-- S ::= expr
-- expr ::= atom | "(" expr ")"
-- atom ::= number | symbol
-- number ::= [0-9]+
-- symbol ::= [A-Za-z_-][A-Za-z_-0-9]+

-- take in a [Token] and produce an AST, which should
-- be syntactically correct, otherwise produce an error

module Parser (
    Expr(Unit, Number, Boolean, Pair, Fst, Snd, Add, If, Var, Let, Fun, Closure, Call, IsUnit, Gt, Lt, Eq),
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

-- Since we don't know if the Symbol is a special keyword
-- or a user-defined binding we need to branch based on that
parseSymbol :: [Token] -> Either String (Expr, [Token])
parseSymbol [] = Left "Expected Symbol but received EOF"
parseSymbol (Sym s:toks) =
    case s of
        -- "let"
        -- "if"
        -- "fst"
        -- "snd"
        -- "+"
        -- "*"
        _ -> Right (Var s, toks)
parseSymbol (tok:_) = Left ("Expected Symbol but received: " ++ (show tok))

parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr [] = Left "Unexpected EOF"
parseExpr (Num v:toks) = Right (Number v, toks)
parseExpr (Sym s:toks) = parseSymbol (Sym s:toks)
parseExpr (LParn:toks) =
    case peek toks of
        Just (Sym s) -> parseSymbol (Sym s:toks) -- parseSymbol consumes next token
        Just LParn -> parseExpr toks
        Just tok -> Left ("Expected LParn or Symbol but received: " ++ (show tok))
        Nothing -> Left "Unexpected EOF"
-- parseExpr RParn:toks =


parse :: [Token] -> Either String (Expr, [Token])
parse toks = parseExpr toks

--parse :: [Token] -> Either Expr
--parse toks = parseExpr toks
--parse [] = Left "Unexpected EOF"
--parse (Num v):toks = (Number v):parse toks
--parse (Sym s):toks = (Var s):parse toks
--parse LParn:toks = 
--    let next = peek toks
--    in case next of
--        (Sym s)

main = putStrLn $ show $ parseExpr [LParn, Num 10]