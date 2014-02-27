module Eval (eval, hlist_to_slist, slist_to_hlist) where

import Control.Applicative
import Debug.Trace

import Parser

import Utils



-- when modules are supported, move these to an
-- implementation written in slang itself

slang_ispair :: Expr
slang_ispair = Closure [] "" [ArgNamed "x"] (IsPair (Var "x"))

-- this might be faster to keep implemented directly
-- in haskell code
-- note: ArgRest handles the conversion of a list of
-- argument expressions to a slang list
slang_list :: Expr
slang_list = Closure [] "" [ArgRest "xs"] (Var "xs")

-- a list in slang is defined as a nested pair whose
-- final 'snd' element is Unit (nil)
-- NOTE: might be inefficient on large lists, if there
-- is a way to write this directly in haskell and just
-- check the final 'snd' pair using haskell pattern matching
-- this would be a lot faster
slang_islist :: Expr
slang_islist = 
    (Closure [] "list?" [ArgNamed "x"] 
        (If (IsPair (Var "x")) 
            (If (IsUnit (Snd (Var "x"))) 
                (Boolean True) 
                (Call (Var "list?") [(Snd (Var "x"))])) 
            (Boolean False)))

slang_map :: Expr
slang_map = 
    (Closure [] "map" [ArgNamed "f", ArgNamed "xs"] 
        (If (IsUnit (Var "xs"))
            Unit 
            (Pair (Call (Var "f") [Fst (Var "xs")])
                  (Call (Var "map") [(Var "f"), Snd (Var "xs")]))))

slang_foldl :: Expr
slang_foldl =
    (Closure [] "foldl" [ArgNamed "f", ArgNamed "base", ArgNamed "xs"]
        (If (IsUnit (Var "xs"))
            (Var "base")
            (Call (Var "foldl") [Var "f", (Call (Var "f") [Var "base", (Fst (Var "xs"))]), (Snd (Var "xs"))])))

slang_foldr :: Expr
slang_foldr =
    (Closure [] "foldr" [ArgNamed "f", ArgNamed "base", ArgNamed "xs"]
        (If (IsUnit (Var "xs"))
            (Var "base")
            (Call (Var "f") [Fst (Var "xs"), (Call (Var "foldr") [Var "f", Var "base", (Snd (Var "xs"))])])))

slang_len :: Expr
slang_len =
    (Closure [] "len" [ArgNamed "xs"]
        (If (IsUnit (Var "xs"))
            (Number 0)
            (Add (Number 1) (Call (Var "len") [Snd (Var "xs")]))))

slang_min :: Expr
slang_min =
    (Closure [] "" [ArgNamed "x", ArgRest "xs"]
        (If (IsUnit (Var "xs"))
            (Var "x")
            (Call slang_foldl [Fun "" [ArgNamed "acc", ArgNamed "y"] (If (Lt (Var "y") (Var "acc")) (Var "y") (Var "acc")), (Var "x"), (Var "xs")])))

slang_max :: Expr
slang_max =
    (Closure [] "" [ArgNamed "x", ArgRest "xs"]
        (If (IsUnit (Var "xs"))
            (Var "x")
            (Call slang_foldl [Fun "" [ArgNamed "acc", ArgNamed "y"] (If (Gt (Var "y") (Var "acc")) (Var "y") (Var "acc")), (Var "x"), (Var "xs")])))

-- Built-in functions

slang_fst :: Expr
slang_fst = Closure [] "" [ArgNamed "pr"] (Fst (Var "pr"))

slang_snd :: Expr
slang_snd = Closure [] "" [ArgNamed "pr"] (Snd (Var "pr"))

slang_add :: Expr
slang_add = 
    (Closure [] "" [ArgNamed "x", ArgRest "xs"]
        (If (IsUnit (Var "xs"))
            (Var "x")
            (Call slang_foldl [Fun "" [ArgNamed "acc", ArgNamed "y"] (Add (Var "acc") (Var "y")), (Var "x"), (Var "xs")])))

slang_sub :: Expr
slang_sub = 
    (Closure [] "" [ArgNamed "x", ArgRest "xs"]
        (If (IsUnit (Var "xs"))
            (Sub (Number 0) (Var "x"))
            (Call slang_foldl [Fun "" [ArgNamed "acc", ArgNamed "y"] (Sub (Var "acc") (Var "y")), (Var "x"), (Var "xs")])))

slang_mul :: Expr
slang_mul = 
    (Closure [] "" [ArgNamed "x", ArgRest "xs"]
        (If (IsUnit (Var "xs"))
            (Var "x")
            (Call slang_foldl [Fun "" [ArgNamed "acc", ArgNamed "y"] (Mul (Var "acc") (Var "y")), (Var "x"), (Var "xs")])))

slang_div :: Expr
slang_div = 
    (Closure [] "" [ArgNamed "x", ArgRest "xs"]
        (If (IsUnit (Var "xs"))
            (Var "x")
            (Call slang_foldl [Fun "" [ArgNamed "acc", ArgNamed "y"] (Div (Var "acc") (Var "y")), (Var "x"), (Var "xs")])))

slang_eq :: Expr
slang_eq = 
    (Closure [] "" [ArgNamed "x", ArgRest "xs"]
        (If (IsUnit (Var "xs"))
            (Boolean True)
            (If 
                (Call slang_foldl [
                    Fun "" [ArgNamed "acc", ArgNamed "y"] 
                        (If (Var "acc") 
                            (If (Eq (Var "y") (Var "acc")) 
                                (Var "y") 
                                (Boolean False))
                            (Boolean False)), 
                    (Var "x"), 
                    (Var "xs")])
                (Boolean True) 
                (Boolean False))))

slang_gt :: Expr
slang_gt = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Gt (Var "x") (Var "y"))

slang_lt :: Expr
slang_lt = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Lt (Var "x") (Var "y"))

slang_not :: Expr
slang_not = Closure [] "" [ArgNamed "x"] (Not (Var "x"))

slang_cons :: Expr
slang_cons = Closure [] "" [ArgNamed "fst", ArgNamed "snd"] (Pair (Var "fst") (Var "snd"))

slang_isnil :: Expr
slang_isnil = Closure [] "" [ArgNamed "x"] (IsUnit (Var "x"))

slang_str :: Expr
slang_str = Closure [] "" [ArgNamed "x"] (StrCons (Var "x"))


-- helper functions
all_or_none :: [Expr] -> Env -> Result [Expr]
all_or_none [] _ = return []
all_or_none (e:es) env = do
    applied <- evalenv e env
    rest <- all_or_none es env
    return (applied:rest)

-- convert a haskell list to a slang list
hlist_to_slist :: [Expr] -> Expr
hlist_to_slist es = foldr (\e ls -> Pair e ls) Unit es

-- convert a slang list to a haskell list
slist_to_hlist :: Expr -> [Expr]
slist_to_hlist Unit = []
slist_to_hlist (Pair e1 e2) = e1:slist_to_hlist e2

-- convert an expression value to a string
to_string :: Expr -> Result Expr
to_string Unit = return (Str "nil")
to_string (Number i) = return (Str (show i))
to_string (Str s) = return (Str s)
to_string (Boolean b) = return (Str (if b then "#t" else "#f"))
to_string (Fun _ _ _) = return (Str "fn")
to_string (Closure _ _ _ _) = return (Str "fn")
to_string (Pair v1 v2) = 
    case (to_string v1, to_string v2) of
        (Ok (Str s1), Ok (Str s2)) -> return (Str ("(" ++ s1 ++ "," ++ s2 ++ ")"))
        (Err e, _) -> Err e
        (_, Err e) -> Err e
to_string _ = Err "Cannot convert expression to string"


-- having an environment represented with a list of (String, Expr) pairs
-- automatically achieves 'shadowing' for bound variables if you add and
-- look up variables to/from the front of the list
envlookup :: String -> Env -> Maybe Expr
envlookup _ []   = Nothing
envlookup s ((v, e):xs) = if s == v then Just e else (envlookup s xs)

evalenv :: Expr -> Env -> Result Expr
evalenv Unit _ = return Unit
evalenv (Number i) _  = return $ Number i
evalenv (Chr c) _ = return $ Chr c
evalenv (Str s) _ = return $ Str s
evalenv (Boolean b) _ = return $ Boolean b
evalenv (Fun name args body) env = return $ Closure env name args body
evalenv (Closure env name args body) _ = return $ Closure env name args body

evalenv (Pair e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok v1, Ok v2) -> return $ Pair v1 v2
        (Err s, _) -> Err s
        (_, Err s) -> Err s

evalenv (IsPair e) env =
    case evalenv e env of
        Ok (Pair _ _) -> return $ Boolean True
        Ok _ -> return $ Boolean False
        Err s -> Err s

evalenv (Fst e) env =
    case evalenv e env of
        Ok (Pair e1 _) -> return e1
        Err s -> Err s
        _ -> Err "Fst got something that isn't a Pair"

evalenv (Snd e) env =
    case evalenv e env of
        Ok (Pair _ e2) -> return e2
        Err s -> Err s
        _ -> Err "Snd got something that isn't a Pair"

evalenv (Add e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> return $ Number (v1 + v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Add"

evalenv (Sub e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> return $ Number (v1 - v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Sub"

evalenv (Mul e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> return $ Number (v1 * v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Mul"

evalenv (Div e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> return $ Number (v1 `div` v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Div"

evalenv (If e1 e2 e3) env = 
    case evalenv e1 env of
        Ok (Boolean False) -> evalenv e3 env
        Ok _ -> evalenv e2 env
        Err s -> Err s

evalenv (Var s) env =
    case envlookup s env of
        Just e -> return e
        Nothing -> Err ("Unbound variable name: " ++ s)

evalenv (Let s e1 e2) env = do
    v <- evalenv e1 env
    evalenv e2 ((s, v):env) -- eval e2 in the augmented environment containing 's'

evalenv (IsUnit e) env =
    case evalenv e env of
        Ok Unit -> return (Boolean True)
        Ok _ -> return (Boolean False)
        Err s -> Err s

evalenv (Gt e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> return $ Boolean (v1 > v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (_, _) -> Err ("Gt received something that wasn't a Number")

evalenv (Lt e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> return $ Boolean (v1 < v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (_, _) -> Err ("Lt received something that wasn't a Number")

evalenv (Eq e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> return $ Boolean (v1 == v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (_, _) -> Err ("Eq received something that wasn't a Number")

evalenv (Not e) env =
    case evalenv e env of
        Ok (Boolean b) -> return $ Boolean (not b)
        Ok _ -> Err ("not applied to Non-boolean")
        Err s -> Err s

-- convert an expression to a Str expression
evalenv (StrCons e) env = do
    v <- evalenv e env
    to_string v

evalenv (Call e1 es) env =
    -- TODO: evaluate args /after/ checking whether the number
    -- of expr parameters can be accepted by the called function
    let applyArgs :: [Arg] -> [Expr] -> Result [(String, Expr)]
        applyArgs [] [] = return []
        applyArgs (ArgRest s:[]) [] = return [(s, Unit)]
        applyArgs (ArgRest s:[]) es = return [(s, hlist_to_slist es)]
        applyArgs (ArgRest s:_) _ = Err "ArgRest can only be at the end of the parameter list."
        applyArgs (ArgNamed s:args) (e:es) = do
            rest <- applyArgs args es
            return ((s,e):rest)
        applyArgs [] es = Err "Called with wrong number of arguments. Too many given."
        applyArgs args [] = Err "Called with wrong number of arguments. Too few given."
    in
    case (evalenv e1 env, all_or_none es env) of
        (Ok (Closure cenv name args body), Ok vals) ->
            case applyArgs args vals of
                Ok bindings -> let newenv = if name == "" 
                                            then bindings ++ cenv
                                            else (name, Closure cenv name args body):bindings ++ cenv
                               in evalenv body newenv
                Err e -> Err e
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (Ok e, _) -> Err ("Call received something that wasn't a Closure: " ++ show e)

prelude :: Env
prelude = [("+", slang_add), ("-", slang_sub), ("*", slang_mul), ("/", slang_div), ("=", slang_eq), (">", slang_gt), ("<", slang_lt), ("not", slang_not), ("cons", slang_cons), ("str", slang_str), ("fst", slang_fst), ("snd", slang_snd), ("nil", Unit), ("nil?", slang_isnil),("list", slang_list), ("pair?", slang_ispair), ("list?", slang_islist), ("map", slang_map), ("#t", Boolean True), ("#f", Boolean False), ("foldl", slang_foldl), ("foldr", slang_foldr), ("len", slang_len), ("min", slang_min), ("max", slang_max)]


eval :: Expr -> Result Expr
eval expr = evalenv expr prelude
