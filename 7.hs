import System.IO (isEOF)
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

contar :: [Char] -> Int
contar str
    |(length (filter (==',') str)) == 0 = casoEspec str
    |otherwise = (length (filter (==',') str)) + 1
casoEspec :: [Char] -> Int
casoEspec str
        |str!!1 == ']' = 0
        |otherwise = 1

main = myLoop "[]"
        
myLoop maior = forever $ do
                done <- isEOF
                when done $  putStrLn maior >> exitSuccess
                str <- getLine
                if((contar str) > (contar maior))
                    then (myLoop str)
                else 
                    (myLoop maior)