
import System.IO

import Interpret

main = do
--    putStr ">>>"
--    hFlush stdout
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ interpret line
            main
