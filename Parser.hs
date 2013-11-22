-- S ::= expr
-- expr ::= atom | "(" expr ")"
-- atom ::= number | symbol
-- number ::= [0-9]+
-- symbol ::= [A-Za-z_-][A-Za-z_-0-9]+

-- take in a [Token] and produce an AST, which should
-- be syntactically correct, otherwise produce an error

module Parser (
    Arg(ArgNamed, ArgRest),
    Expr(Unit,Number,StrLit,Boolean,Pair,Fst,Snd,Add,Sub,Mul,Div,If,Var,Let,Fun,Closure,Call,IsUnit,Gt,Lt,Eq),
    Env, 
    parse)
where

import Control.Applicative
import Debug.Trace

import Lexer

import Utils

data Arg = ArgNamed String | ArgRest String deriving (Show)

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
    | Fun String [Arg] Expr -- empty string for name means anonymous Fun
    | Closure Env String [Arg] Expr -- holds an Env and a Fun
    | Call Expr [Expr]
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

-- parse a cell as a list of data, until reaching an RParn
parseList :: [Token] -> Result ([Expr], [Token])
parseList [] = Err "Expected RParn. Received unexpected EOF"
parseList (RParn:toks) = Ok ([], toks)
parseList toks = 
    case parseExpr toks of
        Ok (e, toks1) -> (\(es, toks2) -> (e:es, toks2)) <$> parseList toks1
        Err s -> Err s

parseArgList :: [Token] -> Result ([Arg], [Token])
parseArgList [] = Err "Expected RParn. Received unexpected EOF"
parseArgList (Sym sym:RParn:toks) = -- check for special ArgRest token
    case sym of
        ('&':rest) -> Ok (ArgRest rest:[], toks)
        ('&':[]) -> Err "'&' cannot be used as a variable name by itself"
        rest -> Ok (ArgNamed rest:[], toks)
parseArgList (RParn:toks) = Ok ([], toks)
parseArgList (Sym sym:toks) = 
    if head sym == '&'
    then Err ("Can only use the var arg form at the end of argument list. In: " ++ (show sym))
    else (\(args, rest) -> (ArgNamed sym:args, rest)) <$> parseArgList toks
parseArgList (tok:toks) = Err ("Expected Symbol. Received: " ++ (show tok))

parseMul :: [Token] -> Result (Expr, [Token])
parseMul toks =
    case parseList toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Mul x y) z -> Mul (Mul x y) z) (Mul a b) xs, rest)
                               a:[] -> Ok (Mul a Unit, rest)
                               _ -> Err "Mul takes at least one argument"
        Err s -> Err s

parseDiv :: [Token] -> Result (Expr, [Token])
parseDiv toks =
    case parseList toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Div x y) z -> Div (Div x y) z) (Div a b) xs, rest)
                               a:[] -> Ok (Div a Unit, rest)
                               _ -> Err "Div takes at least one argument"
        Err s -> Err s

parseAdd :: [Token] -> Result (Expr, [Token])
parseAdd toks =
    case parseList toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Add x y) z -> Add (Add x y) z) (Add a b) xs, rest)
                               a:[] -> Ok (Add a Unit, rest)
                               _ -> Err "Add takes at least one argument"
        Err s -> Err s

parseSub :: [Token] -> Result (Expr, [Token])
parseSub toks =
    case parseList toks of
        Ok (args, rest) -> case args of
                               a:b:xs -> Ok (foldl (\(Sub x y) z -> Sub (Sub x y) z) (Sub a b) xs, rest)
                               a:[] -> Ok (Sub a Unit, rest)
                               _ -> Err "Sub takes at least one argument"
        Err s -> Err s

parseFun :: [Token] -> Result (Expr, [Token])
parseFun (LParn:toks) =
    case parseArgList toks of
        Ok (args, rest) -> case parseExpr rest of
                               Ok (body, rest1) -> case rest1 of
                                                       RParn:rest2 -> Ok (Fun "" args body, rest2) -- consume RParn at end of Fun definition
                                                       tok:rest2 -> Err ("Expected RParn. Received: " ++ (show tok))
                               Err s -> Err s
        Err s -> Err s
parseFun (tok:toks) = Err ("Expected LParn but received: " ++ (show tok))

--parseLet :: [Token] -> Either String (Expr, [Token])
--parseLet toks =
--    
--    case parseArgs toks of
--        Right (args, rest) -> case args of
                                  

-- new parseSymbol function to be used with parseCall
-- should only return things that can be used in a Call expr
parseSymbol :: [Token] -> Result (Expr, [Token])
parseSymbol (Sym sym:toks) =
    case sym of
        _ -> Ok (Var sym, toks)

-- parse a cell as a function call
parseCall :: [Token] -> Result (Expr, [Token])
parseCall [] = Err "Expected Symbol or LParn but received EOF"
parseCall (LParn:toks) = 
    case parseExpr (LParn:toks) of
        Ok (e, rest) -> (\(es, ts) -> (Call e es, ts)) <$> parseList rest
        Err s -> Err s
parseCall (Sym sym:toks) =
    case sym of
        -- we parse special forms here as we don't want to 'Call' these
        -- "cons" -- create a Pair
        -- "let"
        -- "if"
        "fn" -> parseFun toks
        _ -> case parseSymbol (Sym sym:toks) of
                 Ok (e, rest) -> (\(es, ts) -> (Call e es, ts)) <$> parseList rest
                 Err s -> Err s
parseCall (tok:_) = Err ("Expected Symbol, LParn or Var but received: " ++ (show tok))

parseExpr :: [Token] -> Result (Expr, [Token])
parseExpr [] = Err "Unexpected EOF"
parseExpr (Num v:toks) = Ok (Number v, toks)
parseExpr (Str s:toks) = Ok (StrLit s, toks)
parseExpr (Sym s:toks) = Ok (Var s, toks)
parseExpr (LParn:toks) = parseCall toks

parse :: [Token] -> Result Expr
parse toks = (\(e, _) -> e) <$> parseExpr toks

main = putStrLn $ show $ parseExpr [LParn, Sym "+", Num 10, Num 10, RParn]
