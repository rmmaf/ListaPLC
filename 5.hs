checa :: [Char] -> Char -> Int -> [Int]
checa [] char cont = []
checa str char cont
    |(head str) == char = [cont] ++ (checa (tail str) char (cont + 1))
    |otherwise = (checa (tail str) char (cont + 1))

toChar :: [Char] -> Char
toChar [a] = a
main:: IO()
main = do
    str <- getLine
    in2 <- getLine
    let char = (toChar in2)
    print (checa str char 0)