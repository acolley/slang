-- S ::= expr
-- expr ::= "(" (atom | expr) ")"
-- atom ::= number | symbol
-- number ::= [0-9]+
-- symbol ::= [A-Za-z_-][A-Za-z_-0-9]+

data Token =
    LParn
    | RParn
    | Num Int
    | Sym String
    deriving (Show)

lex :: Text -> [Token]
lex null = []
lex c:txt =
    case c of
        '(' -> LParn:lex txt
        ')' -> RParn:lex txt
        ch ->
        '0'..'9' -> ([c] ++ takeWhile (\x -> case x of '0'..'9') txt) ++ lex (dropWhile (\x -> case x of '0'..'9') txt)
        _ -> lex txt

main = putStrLn $ lex "()"
