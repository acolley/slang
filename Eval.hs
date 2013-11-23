module Eval (eval, hlist_to_slist, slist_to_hlist) where

import Control.Applicative
import Debug.Trace

import Parser

import Utils

-- Built-in functions

slang_fst :: Expr
slang_fst = Closure [] "" [ArgNamed "pr"] (Fst (Var "pr"))

slang_snd :: Expr
slang_snd = Closure [] "" [ArgNamed "pr"] (Snd (Var "pr"))

slang_add :: Expr
slang_add = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Add (Var "x") (Var "y"))

slang_sub :: Expr
slang_sub = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Sub (Var "x") (Var "y"))

slang_mul :: Expr
slang_mul = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Mul (Var "x") (Var "y"))

slang_div :: Expr
slang_div = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Div (Var "x") (Var "y"))

slang_eq :: Expr
slang_eq = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Eq (Var "x") (Var "y"))

slang_gt :: Expr
slang_gt = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Gt (Var "x") (Var "y"))

slang_lt :: Expr
slang_lt = Closure [] "" [ArgNamed "x", ArgNamed "y"] (Lt (Var "x") (Var "y"))

slang_cons :: Expr
slang_cons = Closure [] "" [ArgNamed "fst", ArgNamed "snd"] (Pair (Var "fst") (Var "snd"))

slang_isnil :: Expr
slang_isnil = Closure [] "" [ArgNamed "x"] (IsUnit (Var "x"))

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


-- helper functions
all_or_none :: [Expr] -> Env -> Result [Expr]
all_or_none [] _ = Ok []
all_or_none (e:es) env =
    case evalenv e env of
        Ok applied -> case all_or_none es env of
                          Ok rest -> Ok (applied:rest)
                          Err s -> Err s
        Err s -> Err s

-- convert a haskell list to a slang list
hlist_to_slist :: [Expr] -> Expr
hlist_to_slist es = foldr (\e ls -> Pair e ls) Unit es

-- convert a slang list to a haskell list
slist_to_hlist :: Expr -> [Expr]
slist_to_hlist Unit = []
slist_to_hlist (Pair e1 e2) = e1:slist_to_hlist e2


-- having an environment represented with a list of (String, Expr) pairs
-- automatically achieves 'shadowing' for bound variables if you add and
-- look up variables to/from the front of the list
envlookup :: String -> Env -> Maybe Expr
envlookup _ []   = Nothing
envlookup s ((v, e):xs) = if s == v then Just e else (envlookup s xs)

evalenv :: Expr -> Env -> Result Expr
evalenv Unit _ = Ok Unit
evalenv (Number i) _  = Ok (Number i)
evalenv (Chr c) _ = Ok (Chr c)
evalenv (StrLit s) _ = Ok (StrLit s)
evalenv (Boolean b) _ = Ok (Boolean b)
evalenv (Fun name args body) env = Ok (Closure env name args body)
evalenv (Closure env name args body) _ = Ok (Closure env name args body)

evalenv (Pair e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok v1, Ok v2) -> Ok (Pair v1 v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s

evalenv (IsPair e) env =
    case evalenv e env of
        Ok (Pair _ _) -> Ok (Boolean True)
        Ok _ -> Ok (Boolean False)
        Err s -> Err s

evalenv (Fst e) env =
    case evalenv e env of
        Ok (Pair e1 _) -> Ok e1
        Err s -> Err s
        _ -> Err "Fst got something that isn't a Pair"

evalenv (Snd e) env =
    case evalenv e env of
        Ok (Pair _ e2) -> Ok e2
        Err s -> Err s
        _ -> Err "Snd got something that isn't a Pair"

evalenv (Add e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Number (v1 + v2))
        (Ok (Number v1), Ok Unit) -> Ok (Number v1)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Add"

evalenv (Sub e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Number (v1 - v2))
        (Ok (Number v1), Ok Unit) -> Ok (Number (-v1))
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Sub"

evalenv (Mul e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Number (v1 * v2))
        (Ok (Number v1), Ok Unit) -> Ok (Number v1)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Mul"

evalenv (Div e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Number (v1 `div` v2))
        (Ok (Number v1), Ok Unit) -> Ok (Number v1)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-Number used in Div"

evalenv (If e1 e2 e3) env = 
    case (evalenv e1 env) of
        Ok (Boolean b) -> if b then (evalenv e2 env) else (evalenv e3 env)
        Err s -> Err s
        _ -> Err "Non-boolean used as If predicate"

evalenv (Var s) env =
    case (envlookup s env) of
        Just e -> Ok e
        Nothing -> Err ("Unbound variable name: " ++ s)

evalenv (Let s e1 e2) env =
    case evalenv e1 env of
        Ok v -> evalenv e2 ((s, v):env) -- eval e2 in the augmented environment containing 's'
        Err s -> Err s

evalenv (IsUnit e) env =
    case evalenv e env of
        Ok Unit -> Ok (Boolean True)
        Ok _ -> Ok (Boolean False)
        Err s -> Err s

evalenv (Gt e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Boolean (v1 > v2))
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (_, _) -> Err ("Gt received something that wasn't a Number")

evalenv (Lt e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Boolean (v1 < v2))
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (_, _) -> Err ("Lt received something that wasn't a Number")
        
evalenv (Eq e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Boolean (v1 == v2))
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (_, _) -> Err ("Eq received something that wasn't a Number")

evalenv (Call e1 es) env =
    -- TODO: evaluate args /after/ checking whether the number
    -- of expr parameters can be accepted by the called function
    let applyArgs :: [Arg] -> [Expr] -> Result [(String, Expr)]
        applyArgs [] [] = Ok []
        applyArgs (ArgRest s:[]) [] = Ok [(s, Unit)]
        applyArgs (ArgRest s:[]) es = Ok [(s, hlist_to_slist es)]
        applyArgs (ArgRest s:_) _ = Err "ArgRest can only be at the end of the parameter list."
        applyArgs (ArgNamed s:args) (e:es) = case applyArgs args es of
                                                 Ok rest -> Ok ((s, e):rest)
                                                 Err e -> Err e
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

eval :: Expr -> Result Expr
eval expr =
    let env = [("+", slang_add), ("-", slang_sub), ("*", slang_mul), ("/", slang_div), ("=", slang_eq), (">", slang_gt), ("<", slang_lt), ("cons", slang_cons), ("fst", slang_fst), ("snd", slang_snd), ("nil", Unit), ("nil?", slang_isnil),("list", slang_list), ("pair?", slang_ispair), ("list?", slang_islist), ("map", slang_map), ("#t", Boolean True), ("#f", Boolean False)]
    in evalenv expr env
