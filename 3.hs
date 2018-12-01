soma :: [Char] -> Int
soma "" = 0
soma string = ((read ([head string])::Int)) + (soma (tail string))
main :: IO()
main = do
    entrada <- getLine
    print (soma entrada)