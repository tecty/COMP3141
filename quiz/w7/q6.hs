import Data.Char
a = do x <- getLine
       putStrLn (filter isDigit x)
       a 