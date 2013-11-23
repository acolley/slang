
import System.Environment

import Interpret
import Lexer
import Parser
import Eval

import Utils

assert :: Bool -> String -> IO ()
assert True name = putStrLn ("SUCCESS: " ++ name)
assert False name = putStrLn ("FAILED: " ++ name)

testLexer :: IO ()
testLexer = do
    assert ((lexer "(+ 20 20)") == (Ok [LParn, Sym "+", Num 20, Num 20, RParn])) "lexer (+ 20 20)"

testParser :: IO ()
testParser = do
    assert ((parse [Num 10]) == (Ok (Number 10))) "parse [Num 10]"

testEval :: IO ()
testEval = do
    assert ((eval (Number 10)) == (Ok (Number 10))) "eval (Number 10)"
    assert ((eval (Call (Var "snd") [(Pair (Number 10) Unit)])) == (Ok (Unit))) "test snd"

testStack :: IO ()
testStack = do
    assert ((interpret "(* (+ 1 2) 3)") == (Ok (Number 9))) "(* (+ 1 2) 3 4)"
    assert ((interpret "(- 10 20)") == (Ok (Number (-10)))) "(- 10 20)"

-- main = putStrLn $ show $ lexer "(+ 20 20)"
--main = putStrLn $ show $ parseArgs [Num 10, Num 20, Num 30, Num 40, Sym "Poo", RParn]
--main = putStrLn $ show $ parseAdd [Num 10, Num 20, Num 30, Num 40, Num 50, RParn]
--main = putStrLn $ show $ (eval `ext` (parse $ lexer "(* (+ 1 2) 3 4)"))
--main = putStrLn $ show $ (eval `ext` (parse $ lexer "(- 10 20 (* 10 10))"))
--main = putStrLn $ show $ eval (Call (Var "snd") (Pair (Number 10) Unit)) -- test global funcs "fst" and "snd"
--main = putStrLn $ show $ eval (Call (Call (Var "cons") (Number 10)) (Number 20))
--main = putStrLn $ show $ parseSyntax [LParn, LParn, Num 10, RParn, RParn]
--main = putStrLn $ show $ eval (Call (Fun "" ["x", "y"] (Add (Var "x") (Var "y"))) [Number 10, Number 20]) -- test multi argument functions
--main = putStrLn $ show $ eval (Call (Var "+") [Number 1, Number 1])
--main = putStrLn $ show $ hlist_to_slist [Number 10, Number 20, Number 30]
--main = putStrLn $ show $ slist_to_hlist (hlist_to_slist [Number 10, Number 20, Number 30])
--main = putStrLn $ show $ eval (Call (Var "nil?") [(Unit)])
--main = putStrLn $ show $ eval (Call (Var "list") [Number 1, Number 2, Number 3])
--main = putStrLn $ show $ eval (Call (Var "pair?") [Pair (Number 1) (Number 2)])
--main = putStrLn $ show $ eval (Call (Var "=") [Number 1, Number 1])
--main = putStrLn $ show $ eval (Call (Var "list?") [(hlist_to_slist [Number 1, Number 2, Number 3])])
--main = putStrLn $ show $ eval (Call (Var "map") [(Fun "" [ArgNamed "x"] (Add (Var "x") (Number 1))), (hlist_to_slist [Number 1, Number 2, Number 3])])

main = do
    testLexer
    testParser
    testEval
    testStack