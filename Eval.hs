module Eval (eval) where

import Parser

-- having an environment represented with a list of (String, Expr) pairs
-- automatically achieves 'shadowing' for bound variables if you add and
-- look up variables to/from the front of the list
envlookup :: String -> Env -> Maybe Expr
envlookup _ []   = Nothing
envlookup s ((v, e):xs) = if s == v then Just e else (envlookup s xs)

evalenv :: Expr -> Env -> Either String Expr
evalenv Unit _ = Right Unit
evalenv (Number i) _  = Right (Number i)
evalenv (Boolean b) _ = Right (Boolean b)
evalenv (Pair e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right v1, Right v2) -> Right (Pair v1 v2)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
evalenv (Fst e) env =
    case evalenv e env of
        Right (Pair e1 _) -> Right e1
        Left err -> Left err
        _ -> Left "Fst got something that isn't a Pair"
evalenv (Snd e) env =
    case evalenv e env of
        Right (Pair _ e2) -> Right e2
        Left err -> Left err
        _ -> Left "Snd got something that isn't a Pair"
evalenv (Add e1 e2) env = 
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Number (v1 + v2))
        (Right (Number v1), Right Unit) -> Right (Number v1)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        _ -> Left "Non-number used in Add"
evalenv (Sub e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Number (v1 - v2))
        (Right (Number v1), Right Unit) -> Right (Number (-v1))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        _ -> Left "Non-number used in Sub"
evalenv (Mul e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Number (v1 * v2))
        (Right (Number v1), Right Unit) -> Right (Number v1)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        _ -> Left "Non-number used in Mul"
evalenv (Div e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Number (v1 `div` v2))
        (Right (Number v1), Right Unit) -> Right (Number v1)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        _ -> Left "Non-number used in Div"
evalenv (If e1 e2 e3) env = 
    case (evalenv e1 env) of
        Right (Boolean b) -> if b then (evalenv e2 env) else (evalenv e3 env)
        Left err -> Left err
        _ -> Left "Non-boolean used as If predicate"
evalenv (Var s) env =
    case (envlookup s env) of
        Just e -> Right e
        Nothing -> Left "Unbound variable name"
evalenv (Let s e1 e2) env =
    case evalenv e1 env of
        Right v -> evalenv e2 ((s, v):env) -- eval e2 in the augmented environment containing 's'
        Left err -> Left err
evalenv (Fun name arg body) env = Right (Closure env name arg body)
evalenv (Closure env name arg body) _ = Right (Closure env name arg body)
evalenv (Call e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Closure cenv name arg body), Right val) ->
            let newenv = if name == "" then (arg, val):cenv
                         else (name, Closure cenv name arg body):(arg, val):cenv
            in evalenv body newenv
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right e, _) -> Left ("Call received something that wasn't a Closure: " ++ show e)
evalenv (IsUnit e) env =
    case evalenv e env of
        Right Unit -> Right (Boolean True)
        Right _ -> Right (Boolean False)
        Left err -> Left err
evalenv (Gt e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Boolean (v1 > v2))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (_, _) -> Left ("Gt received something that wasn't a Number")
evalenv (Lt e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Boolean (v1 < v2))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (_, _) -> Left ("Lt received something that wasn't a Number")
evalenv (Eq e1 e2) env =
    case (evalenv e1 env, evalenv e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Boolean (v1 == v2))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (_, _) -> Left ("Eq received something that wasn't a Number")

eval :: Expr -> Either String Expr
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
