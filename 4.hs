palindromo :: Ord a => [a] -> Bool
palindromo [a] = True
palindromo [] = True
palindromo [a, b]
    |a == b = True
    |otherwise = False
palindromo str
    |(last str) == (head str) = palindromo (init (tail str))
    |otherwise = False

toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

main :: IO()
main = do
    entrada <- getLine
    let check = (palindromo (toBin (read entrada::Int)) )
    if(check)
        then do print "True"
    else
        print "False"