
import System.Environment

import Interpret

main = do
    args <- getArgs
    lines <- readFile $ head args
    putStrLn $ interpret $ lines
