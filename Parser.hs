-- S ::= expr
-- expr ::= atom | "(" expr ")"
-- atom ::= number | symbol
-- number ::= [0-9]+
-- symbol ::= [A-Za-z_-][A-Za-z_-0-9]+

-- take in a [Token] and produce an AST, which should
-- be syntactically correct, otherwise produce an error

module Parser (
    Expr(Unit,Number,StrLit,Boolean,Pair,Fst,Snd,Add,Sub,Mul,Div,If,Var,Let,Fun,Closure,Call,IsUnit,Gt,Lt,Eq),
    Env, 
    parse) 
where

import Control.Applicative

import Lexer

import Utils

data Expr = 
    Unit
    | Number Int
    | StrLit String
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
parseArgs :: [Token] -> Result ([Expr], [Token])
parseArgs (RParn:toks) = Ok ([], toks)
parseArgs toks = 
    case parseExpr toks of
        Ok (e, toks1) -> (\(es, toks2) -> (e:es, toks2)) <$> parseArgs toks1
        Err s -> Err s

parseMul :: [Token] -> Result (Expr, [Token])
parseMul toks =
    case parseArgs toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Mul x y) z -> Mul (Mul x y) z) (Mul a b) xs, rest)
                               a:[] -> Ok (Mul a Unit, rest)
                               _ -> Err "Mul takes at least one argument"
        Err s -> Err s

parseDiv :: [Token] -> Result (Expr, [Token])
parseDiv toks =
    case parseArgs toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Div x y) z -> Div (Div x y) z) (Div a b) xs, rest)
                               a:[] -> Ok (Div a Unit, rest)
                               _ -> Err "Div takes at least one argument"
        Err s -> Err s

parseAdd :: [Token] -> Result (Expr, [Token])
parseAdd toks =
    case parseArgs toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Add x y) z -> Add (Add x y) z) (Add a b) xs, rest)
                               a:[] -> Ok (Add a Unit, rest)
                               _ -> Err "Add takes at least one argument"
        Err s -> Err s

parseSub :: [Token] -> Result (Expr, [Token])
parseSub toks =
    case parseArgs toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Sub x y) z -> Sub (Sub x y) z) (Sub a b) xs, rest)
                               a:[] -> Ok (Sub a Unit, rest)
                               _ -> Err "Sub takes at least one argument"
        Err s -> Err s

parseFun :: [Token] -> Result (Expr, [Token])
parseFun toks =
    case parseArgs toks of
        Ok (args, rest) -> case args of
                               arg:body:[] -> case arg of
                                                  StrLit s -> Ok (Fun "" s body, rest)
                                                  _ -> Err "First argument to Fun should be a string"
                               xs -> Err ("Fun definition takes two arguments: " ++ (show (length xs)) ++ " given")
        Err s -> Err s

--parseLet :: [Token] -> Either String (Expr, [Token])
--parseLet toks =
--    
--    case parseArgs toks of
--        Right (args, rest) -> case args of
                                  

-- Since we don't know if the Symbol is a special keyword
-- or a user-defined binding we need to branch based on that
-- parseSymbol checks specifically for 'special forms' or
-- it simply returns a Var with the Symbol name
parseSymbol :: [Token] -> Result (Expr, [Token])
parseSymbol [] = Err "Expected Symbol but received EOF"
parseSymbol (Sym s:toks) =
    case s of
        -- "cons" -- create a Pair
        -- "let"
        "fn" -> parseFun toks
        -- "if"
        -- "fst"
        -- "snd"
        "+" -> parseAdd toks -- consume '+' Symbol token, TODO: have this as a 'global' scope function?
        "-" -> parseSub toks
        "*" -> parseMul toks -- consume '*' Symbol token
        "/" -> parseDiv toks
        _ -> Ok (Var s, toks)
parseSymbol (tok:_) = Err ("Expected Symbol but received: " ++ (show tok))

parseCall :: [Token] -> Result (Expr, [Token])
parseCall [] = Err "Expected Symbol or LParn but received EOF"
parseCall (LParn:toks) = parseExpr (LParn:toks)
parseCall (Sym sym:toks) =
    case sym of
        -- "cons" -- create a Pair
        -- "let"
        -- "if"
        -- "fst"
        -- "snd"
        _ -> case parseSymbol (Sym sym:toks) of
                 Ok (e, rest) -> (\(e1, ts) -> (Call e e1, ts)) <$> parseExpr rest
                 Err s -> Err s
parseCall (tok:_) = Err ("Expected Symbol, LParn or Var but received: " ++ (show tok))

parseExpr :: [Token] -> Result (Expr, [Token])
parseExpr [] = Err "Unexpected EOF"
parseExpr (Num v:toks) = Ok (Number v, toks)
parseExpr (Str s:toks) = Ok (StrLit s, toks)
parseExpr (Sym s:toks) = Ok (Var s, toks)
--parseExpr (LParn:toks) = parseCall toks
parseExpr (LParn:toks) =
    case peek toks of
        Just (Sym s) -> parseSymbol toks
        Just LParn -> parseExpr toks
        Just tok -> Err ("Expected LParn or Symbol but received: " ++ (show tok))
        Nothing -> Err "Unexpected EOF"

parse :: [Token] -> Result Expr
parse toks = (\(e, _) -> e) <$> parseExpr toks

main = putStrLn $ show $ parseExpr [LParn, Sym "+", Num 10, Num 10, RParn]
