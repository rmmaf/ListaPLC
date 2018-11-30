is_prime :: Int -> Bool
is_prime 0 = False
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
           | otherwise = True


is_square2 :: Int -> Int -> Bool
is_square2 ent init
    |init^2 == ent = True
    |init == ent = False
    |otherwise = is_square2 ent (init + 1)

main :: IO ()
main = do 
    entrada <- getLine
    if(is_prime (read entrada::Int))
        then do putStrLn "Primo"
    else if (is_square2 (read entrada::Int) 0)
        then do putStrLn "Quadrado Perfeito"
    else
        putStrLn "Natural"
