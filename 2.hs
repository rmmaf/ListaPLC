import Data.Char (isUpper)
import Data.Char (toLower)
import Data.Char (isLower)
import Data.Char (toUpper)

inverte:: Char -> Char
inverte char
    |isUpper char = toLower char
    |isLower char = toUpper char
    |otherwise = char
main :: IO()
main = do
    entrada <- getLine
    putStrLn (map (inverte) entrada)