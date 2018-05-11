primeCheck :: Int -> Bool
primeCheck x
    | x > 0 && x <= 3                  = True
    | x `mod` 2 == 0 || x `mod` 3 == 0 = False
    | otherwise                        = primeCheckIter x
