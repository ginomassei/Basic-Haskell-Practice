-- AUX Functions.
fact :: Integer -> Integer
fact 0 = 1  -- End of recursion.
fact n = n * (fact (n - 1))

sin' :: Float -> Integer -> Float
sin' x 0 = x  -- End of recursion.
sin' x n
    | x >= 0 && x <= 6.284
        = ((-1) ^ n / fromIntegral (fact (2*n + 1))) * (x ^ (2*n + 1)) + sin' x (n - 1)
    | otherwise = 2

-- TP Functions.
funcion1 :: Integer -> Integer -> Integer
funcion1 x n 
    | n > 0 = x + 1
    | n < 0 = x - 1
    | otherwise = 0

funcion2 :: [Integer] -> Integer -> [Integer]
funcion2 [] _n = []  -- End of recursion.
funcion2 (x : xs) n = funcion1 x n : funcion2 xs n

funcion3 :: Float -> Integer -> Float
funcion3 _x 0 = 1  -- End of recursion.
funcion3 x n = x ^ n / fromIntegral (fact n) + funcion3 x (n - 1)

funcion4 :: Float -> Float
funcion4 x = sin' x 10

funcion5 :: [Float] -> [Float]
funcion5 list =
    [funcion4 x | x <- list]

funcion6 :: Integer -> Integer -> (Integer, Integer)
funcion6 x n 
    | n > 0 = (x, x + 1)
    | n < 0 = (x, x - 1)
    | otherwise = (0, 0)
