{-|
Alumnos: 
Massei, Gino - 79366
Perlo, Matías - 83723
Lacias, Ignacio - 71348
Gonzales, Facundo - 77494
-}

-- AUX Functions.
fact :: Integer -> Integer
fact 0 = 1  -- End of recursion.
fact n = n * (fact (n - 1))

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

funcion4:: Float -> Float
funcion4 x
    | x < 0 || x > 6.284 = 2
    | otherwise  = sum [((-1) ^ n / fromIntegral (fact (2*n + 1))) * (x ^ (2*n + 1)) | n <- [0..9]]

funcion5 :: [Float] -> [Float]
funcion5 list =
    [funcion4 x | x <- list]

funcion6 :: Integer -> Integer -> (Integer, Integer)
funcion6 x n 
    | n > 0 = (x, x + 1)
    | n < 0 = (x, x - 1)
    | otherwise = (0, 0)
