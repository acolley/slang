module Eval (eval) where

import Parser

import Utils

-- having an environment represented with a list of (String, Expr) pairs
-- automatically achieves 'shadowing' for bound variables if you add and
-- look up variables to/from the front of the list
envlookup :: String -> Env -> Maybe Expr
envlookup _ []   = Nothing
envlookup s ((v, e):xs) = if s == v then Just e else (envlookup s xs)

evalenv :: Expr -> Env -> Result Expr
evalenv Unit _ = Ok Unit
evalenv (Number i) _  = Ok (Number i)
evalenv (Boolean b) _ = Ok (Boolean b)
evalenv (Pair e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok v1, Ok v2) -> Ok (Pair v1 v2)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
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
        _ -> Err "Non-number used in Add"
evalenv (Sub e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Number (v1 - v2))
        (Ok (Number v1), Ok Unit) -> Ok (Number (-v1))
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-number used in Sub"
evalenv (Mul e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Number (v1 * v2))
        (Ok (Number v1), Ok Unit) -> Ok (Number v1)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-number used in Mul"
evalenv (Div e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Number v1), Ok (Number v2)) -> Ok (Number (v1 `div` v2))
        (Ok (Number v1), Ok Unit) -> Ok (Number v1)
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        _ -> Err "Non-number used in Div"
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
evalenv (Fun name arg body) env = Ok (Closure env name arg body)
evalenv (Closure env name arg body) _ = Ok (Closure env name arg body)
evalenv (Call e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Ok (Closure cenv name arg body), Ok val) ->
            let newenv = if name == "" then (arg, val):cenv
                         else (name, Closure cenv name arg body):(arg, val):cenv
            in evalenv body newenv
        (Err s, _) -> Err s
        (_, Err s) -> Err s
        (Ok e, _) -> Err ("Call received something that wasn't a Closure: " ++ show e)
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

eval :: Expr -> Result Expr
eval expr = evalenv expr []

-- TODO: Need some way to support variable numbers of arguments
--slang_add :: Expr
--slang_add = (Fun "" "x")

mymap :: Expr
mymap = 
    (Fun "_map" "f" 
        (Fun "" "xs"
            (If (IsUnit (Var "xs")) 
                Unit 
                (Pair (Call (Var "f") (Fst (Var "xs"))) 
                      (Call (Call (Var "_map") (Var "f")) (Snd (Var "xs")))))))

main = putStrLn $ show $ eval (Eq (Number 10) (Number 10))
