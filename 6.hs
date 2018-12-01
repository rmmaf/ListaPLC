import System.IO (isEOF)
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

checar :: Int -> Int -> [Char] -> Int -> [Char]
checar mini maxi poke forc
    |((mini <= forc) && (forc <= maxi)) = (poke ++ " (" ++ (show forc) ++ ")")
    |otherwise = ""



main :: IO()
main = do
    in1 <- getLine
    in2 <- getLine
    let mini = read in1::Int
    let maxi = read in2::Int
    myLoop mini maxi

myLoop mini maxi = forever $ do
                done <- isEOF
                when done $  return() >> exitSuccess
                poke <- getLine
                in1 <- getLine
                let forca = read in1::Int
                if((checar mini maxi poke forca) /= "")
                    then putStrLn (checar mini maxi poke forca)
                else return()
                myLoop mini maxi