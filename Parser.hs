-- S ::= expr
-- expr ::= atom | "(" expr ")"
-- atom ::= number | symbol
-- number ::= [0-9]+
-- symbol ::= [A-Za-z_-][A-Za-z_-0-9]+

-- take in a [Token] and produce an AST, which should
-- be syntactically correct, otherwise produce an error

module Parser (
    Expr(Unit, Number, Boolean, Pair, Fst, Snd, Add, Sub, Mul, Div, If, Var, Let, Fun, Closure, Call, IsUnit, Gt, Lt, Eq),
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
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
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

-- Return the list of Exprs terminated by an RParn
-- in the given Token list
parseArgs :: [Token] -> Either String ([Expr], [Token])
parseArgs (RParn:toks) = Right ([], toks)
parseArgs toks =
    case parseExpr toks of
        Right (e, toks1) -> case parseArgs toks1 of
                                Right (es, toks2) -> Right (e:es, toks2)
                                Left s -> Left s
        Left s -> Left s

parseMul :: [Token] -> Either String (Expr, [Token])
parseMul toks =
    case parseArgs toks of
        Right (args, rest) -> case args of
                                  a:b:xs -> Right (foldl (\(Mul x y) z -> Mul (Mul x y) z) (Mul a b) xs, rest)
                                  a:[] -> Right (Mul a Unit, rest)
                                  _ -> Left "Mul takes at least one argument"
        Left s -> Left s

parseDiv :: [Token] -> Either String (Expr, [Token])
parseDiv toks =
    case parseArgs toks of
        Right (args, rest) -> case args of
                                  a:b:xs -> Right (foldl (\(Div x y) z -> Div (Div x y) z) (Div a b) xs, rest)
                                  a:[] -> Right (Div a Unit, rest)
                                  _ -> Left "Div takes at least one argument"
        Left s -> Left s

parseAdd :: [Token] -> Either String (Expr, [Token])
parseAdd toks =
    case parseArgs toks of
        Right (args, rest) -> case args of
                                  a:b:xs -> Right (foldl (\(Add x y) z -> Add (Add x y) z) (Add a b) xs, rest)
                                  a:[] -> Right (Add a Unit, rest)
                                  _ -> Left "Add takes at least one argument"
        Left s -> Left s

parseSub :: [Token] -> Either String (Expr, [Token])
parseSub toks =
    case parseArgs toks of
        Right (args, rest) -> case args of
                                  a:b:xs -> Right (foldl (\(Sub x y) z -> Sub (Sub x y) z) (Sub a b) xs, rest)
                                  a:[] -> Right (Sub a Unit, rest)
                                  _ -> Left "Sub takes at least one argument"
        Left s -> Left s

--parseLet :: [Token] -> Either String (Expr, [Token])
--parseLet toks =
--    
--    case parseArgs toks of
--        Right (args, rest) -> case args of
                                  

-- Since we don't know if the Symbol is a special keyword
-- or a user-defined binding we need to branch based on that
-- parseSymbol checks specifically for 'special forms' or
-- it simply returns a Var with the Symbol name
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
        "-" -> parseSub toks
        "*" -> parseMul toks -- consume '*' Symbol token
        "/" -> parseDiv toks
        _ -> Right (Var s, toks)
parseSymbol (tok:_) = Left ("Expected Symbol but received: " ++ (show tok))

parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr [] = Left "Unexpected EOF"
parseExpr (Num v:toks) = Right (Number v, toks)
parseExpr (Sym s:toks) = Right (Var s, toks)
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

main = putStrLn $ show $ parseExpr [LParn, Sym "+", Num 10, Num 10, RParn]
