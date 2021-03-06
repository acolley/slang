-- S ::= expr
-- expr ::= atom | "(" expr ")"
-- atom ::= number | symbol
-- number ::= [0-9]+
-- symbol ::= [A-Za-z_-][A-Za-z_-0-9]+

-- take in a [Token] and produce an AST, which should
-- be syntactically correct, otherwise produce an error

module Parser (
    Arg(ArgNamed, ArgRest),
    Expr(Unit,Number,Chr,Str,StrCons,Boolean,Pair,IsPair,Fst,Snd,Add,Sub,Mul,Div,If,Var,Let,Fun,Closure,Call,IsUnit,Gt,Lt,Eq,Not),
    Env, 
    parse)
where

import Control.Applicative
import Debug.Trace

import Lexer

import Utils

data Arg = ArgNamed String | ArgRest String deriving (Eq, Show)

data Expr = 
    Unit
    | Number Int
    | Symbol String
    | Chr Char
    | Str String
    | StrCons Expr -- convert the expression into a Str expression
    | Boolean Bool
    | Pair Expr Expr
    | IsPair Expr
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
    | Not Expr
    deriving (Eq, Show)

type Env = [(String, Expr)]

data ParseState = ParseState {
  tokens :: [Token]
  } deriving (Show)

-- parse a cell as a list of data, until reaching an RParn
parseList :: [Token] -> Result ([Expr], [Token])
parseList [] = Err "Expected RParn. Received unexpected EOF."
parseList (RParn:toks) = return ([], toks)
parseList toks = do
    (e, toks1) <- parseExpr toks
    (es, toks2) <- parseList toks1
    return (e:es, toks2)

parseArgList :: [Token] -> Result ([Arg], [Token])
parseArgList [] = Err "Expected RParn. Received unexpected EOF"
parseArgList (Sym sym:RParn:toks) = -- check for special ArgRest token
    case sym of
        ('&':[]) -> Err "'&' cannot be used as a variable name by itself."
        ('&':rest) -> return (ArgRest rest:[], toks)
        rest -> return (ArgNamed rest:[], toks)
parseArgList (RParn:toks) = return ([], toks)
parseArgList (Sym sym:toks) = 
    if head sym == '&'
    then Err ("Can only use the var arg form at the end of argument list. In: " ++ (show sym))
    else do
        (args, rest) <- parseArgList toks
        return (ArgNamed sym:args, rest)
parseArgList (tok:toks) = Err ("Expected Symbol. Received: " ++ (show tok))

parseFun :: [Token] -> Result (Expr, [Token])
parseFun (LParn:toks) = do
    (args, rest) <- parseArgList toks
    (body, rest1) <- parseExpr rest
    case rest1 of
        [] -> Err "Expected RParn. Received unexpected EOF"
        (RParn:rest2) -> return (Fun "" args body, rest2) -- consume RParn at end of Fun definition
        (tok:rest2) -> Err ("Expected RParn. Received: " ++ (show tok))
parseFun (tok:toks) = Err ("fn expected LParn but received: " ++ (show tok))

parseIf :: [Token] -> Result (Expr, [Token])
parseIf toks = do
    (args, rest) <- parseList toks
    case args of
        (e1:e2:e3:[]) -> return (If e1 e2 e3, rest)
        _ -> Err ("If expression takes 3 arguments. Received: " ++ (show (length args)))

parseLet :: [Token] -> Result (Expr, [Token])
parseLet (Sym sym:toks) =
    if sym `elem` ["let", "if", "fn", "def", "defn", "import"]
    then Err (sym ++ " is a reserved word. It cannot be used in a let expression.")
    else do
        (args, rest) <- parseList toks
        case args of
            (e1:e2:[]) -> return (Let sym e1 e2, rest)
            _ -> Err ("Let expression takes 3 arguments. Received: " ++ (show ((length args) + 1)))
parseLet (tok:_) = Err ("let expected a Symbol. Received: " ++ (show tok))
parseLet [] = Err "let expected a Symbol. Received unexpected EOF"

-- a 'def' expands to a 'let' over the current tokens
parseDef :: [Token] -> Result (Expr, [Token])
parseDef (Sym sym:toks) =
    if sym `elem` ["let", "if", "fn", "def", "defn", "import"]
    then Err (sym ++ " is a reserved word. It cannot be used in a def.")
    else do
        (args, rest) <- parseList toks
        case args of
            (e1:[]) -> if null rest
                       then return (Unit, [])
                       else do
                           (e2, toks1) <- parseExpr rest
                           return (Let sym e1 e2, toks1)
            _ -> Err ("def takes 2 arguments. Received: " ++ (show ((length args) + 1)))
parseDef (tok:_) = Err ("def expected a Symbol. Received: " ++ (show tok))
parseDef [] = Err "def expected a Symbol. Received unexpected EOF"

parseDefn :: [Token] -> Result (Expr, [Token])
parseDefn (Sym sym:toks) =
    case toks of
        (LParn:rest) -> do
            (args, rest1) <- parseArgList rest
            (body, rest2) <- parseExpr rest1
            case rest2 of
                [] -> Err "defn expected RParn but received EOF"
                (RParn:[]) -> return (Unit, [])
                (RParn:rest3) -> do
                    (e, rest4) <- parseExpr rest3
                    return (Let sym (Fun sym args body) e, rest4)
                (tok:_) -> Err ("defn expected RParn but received: " ++ (show tok))
        (tok:_) -> Err ("defn expected LParn but received: " ++ (show tok))
parseDefn (tok:_) = Err ("defn expected Symbol but received: " ++ (show tok))

-- parse a cell as a function call
parseCall :: [Token] -> Result (Expr, [Token])
parseCall [] = Err "Expected Symbol or LParn but received EOF"
parseCall (LParn:toks) = do
    (e, rest) <- parseExpr (LParn:toks)
    (es, ts) <- parseList rest
    return (Call e es, ts)
parseCall (Sym sym:toks) =
    case sym of
        -- we parse special forms here as we don't want to 'Call' these
        "let" -> parseLet toks
        "def" -> parseDef toks
        "defn" -> parseDefn toks
        "if" -> parseIf toks
        "fn" -> parseFun toks
        _ -> do
            (es, ts) <- parseList toks
            return (Call (Var sym) es, ts)
parseCall (tok:_) = Err ("Call expected Symbol, LParn or Var but received: " ++ (show tok))

parseExpr :: [Token] -> Result (Expr, [Token])
parseExpr [] = Err "Expr: Unexpected EOF"
parseExpr (Num v:toks) = return (Number v, toks)
parseExpr (StrLit s:toks) = return (Str s, toks)
parseExpr (Sym s:toks) = return (Var s, toks)
parseExpr (LParn:toks) = parseCall toks

parse :: [Token] -> Result Expr
parse toks = (\(e, _) -> e) <$> parseExpr toks

