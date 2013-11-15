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

-- having an environment represented with a list of (String, Expr) pairs
-- automatically achieves 'shadowing' for bound variables if you add and
-- look up variables to/from the front of the list
envlookup :: String -> Env -> Maybe Expr
envlookup _ []   = Nothing
envlookup s ((v, e):xs) = if s == v then Just e else (envlookup s xs)

eval :: Expr -> Env -> Either String Expr
eval Unit _ = Right Unit
eval (Number i) _  = Right (Number i)
eval (Boolean b) _ = Right (Boolean b)
eval (Pair e1 e2) env =
    case (eval e1 env, eval e2 env) of
        (Right v1, Right v2) -> Right (Pair v1 v2)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
eval (Fst e) env =
    case eval e env of
        Right (Pair e1 _) -> Right e1
        Left err -> Left err
        _ -> Left "Fst got something that isn't a Pair"
eval (Snd e) env =
    case eval e env of
        Right (Pair _ e2) -> Right e2
        Left err -> Left err
        _ -> Left "Snd got something that isn't a Pair"
eval (Add e1 e2) env = 
    case (eval e1 env, eval e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Number (v1 + v2))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        _ -> Left "Non-number used in Add"
eval (If e1 e2 e3) env = 
    case (eval e1 env) of
        Right (Boolean b) -> if b then (eval e2 env) else (eval e3 env)
        Left err -> Left err
        _ -> Left "Non-boolean used as If predicate"
eval (Var s) env =
    case (envlookup s env) of
        Just e -> Right e
        Nothing -> Left "Unbound variable name"
eval (Let s e1 e2) env =
    case eval e1 env of
        Right v -> eval e2 ((s, v):env) -- eval e2 in the augmented environment containing 's'
        Left err -> Left err
eval (Fun name arg body) env = Right (Closure env name arg body)
eval (Closure env name arg body) _ = Right (Closure env name arg body)
eval (Call e1 e2) env =
    case (eval e1 env, eval e2 env) of
        (Right (Closure cenv name arg body), Right val) ->
            let newenv = if name == "" then (arg, val):cenv
                         else (name, Closure cenv name arg body):(arg, val):cenv
            in eval body newenv
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right e, _) -> Left ("Call received something that wasn't a Closure: " ++ show e)
eval (IsUnit e) env =
    case eval e env of
        Right Unit -> Right (Boolean True)
        Right _ -> Right (Boolean False)
        Left err -> Left err
eval (Gt e1 e2) env =
    case (eval e1 env, eval e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Boolean (v1 > v2))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (_, _) -> Left ("Gt received something that wasn't a Number")
eval (Lt e1 e2) env =
    case (eval e1 env, eval e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Boolean (v1 < v2))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (_, _) -> Left ("Lt received something that wasn't a Number")
eval (Eq e1 e2) env =
    case (eval e1 env, eval e2 env) of
        (Right (Number v1), Right (Number v2)) -> Right (Boolean (v1 == v2))
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (_, _) -> Left ("Eq received something that wasn't a Number")

mymap :: Expr
mymap = 
    (Fun "_map" "f" 
        (Fun "" "xs"
            (If (IsUnit (Var "xs")) 
                Unit 
                (Pair (Call (Var "f") (Fst (Var "xs"))) 
                      (Call (Call (Var "_map") (Var "f")) (Snd (Var "xs")))))))

--main = putStrLn $ show $ eval (Call (Call mymap (Fun "" "x" (Add (Var "x") (Number 1)))) (Pair (Number 10) (Pair (Number 20) Unit))) []
main = putStrLn $ show $ eval (Eq (Number 10) (Number 10)) []
