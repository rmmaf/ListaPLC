import Data.List (subsequences)

strToList :: [Char] -> [Char]-> [Int]
strToList str buffer
    |str == "" = [(read buffer)]
    |(head str) == ' ' = [(read buffer)] ++ (strToList (tail str) (""))
    |otherwise = strToList (tail str) (buffer ++ [head str])

main::IO()
main = do
    in1 <- getLine
    in2 <- getLine
    let lista = strToList in1 ""
    let valor = read in2
    let soma = sum[1 | x <- (subsequences lista), (sum x) == valor]
    print soma