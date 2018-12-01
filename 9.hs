import Data.List

strToList :: [Char] -> [Char]-> [Int]
strToList str buffer
    |str == "" = [(read buffer)]
    |(head str) == ' ' = [(read buffer)] ++ (strToList (tail str) (""))
    |otherwise = strToList (tail str) (buffer ++ [head str])

escreveMatriz i matriz = do
    if(i == 0)
        then return matriz
    else do
        inp <- getLine
        let linha = strToList inp ""
        escreveMatriz (i-1) (matriz ++ [linha])

lineToString ::[Int] -> [Char]
lineToString [] = ""
lineToString line = (show (head line)) ++ " " ++ (lineToString (tail line))

printMatrix i matrix = do
    if(i == 0)
        then return()
    else do
        let linha = (lineToString (head matrix))
        putStrLn linha
        printMatrix (i-1) (tail matrix)
mmult :: [[Int]] -> [[Int]] -> [[Int]] 
mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]
main :: IO()
main = do
    in1 <- getLine
    let i1 = read in1::Int
    in2 <- getLine
    let j1 = read in2::Int
    mat1 <- escreveMatriz i1 []
    in3 <- getLine
    let i2 = read in3::Int
    in4 <- getLine
    let j2 = read in4::Int
    mat2 <- escreveMatriz i2 []
    let result = mmult mat1 mat2
    printMatrix i1 result
    