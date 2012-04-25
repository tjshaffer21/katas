{-
 - Project Euler
 - Problem 3
 - The prime factors of 13195 are 5,7,13,29.
 - What is the largest prime factor of the number 600851475143?
 -}
import System (getArgs)

--  Sieve of Eratosthenes
sieveE :: Int -> [Int]
sieveE n = 2:findPrimes [3,5..n]

-- Find all primes of given list.
findPrimes :: [Int] -> [Int]
findPrimes []     = []
findPrimes (x:xs) = x:(findPrimes (dropMult x xs))

-- Remove multiples of n from xs
dropMult :: Int -> [Int] -> [Int]
dropMult n [] = []
dropMult n (x:xs)
    | mod x n == 0 = dropMult n xs
    | otherwise    = x:dropMult n xs

-- Trial Division
-- Slow for significantly large n (> 98765432112340)
trialDiv :: Int -> [Int]
trialDiv n
    | n < 1     = []
    | n == 1    = [1]
    | otherwise = 
        let xs = trialDiv' (sieveE . ceiling $ (fromIntegral n ** 0.5)) n
        in let ys = [x | x <- xs, x /= n] 
        in if length ys > 1 then ys else [1,n]

-- Recurse through list of primes
trialDiv' :: [Int] -> Int -> [Int]
trialDiv' [] n     = []
trialDiv' (x:xs) n
    | x*x > n      = []
    | otherwise    = let ys = factor n x
                         zs = trialDiv' xs (last ys)
                     in if length zs > 1 then (init ys) ++ zs else ys 

-- Check if p is factor of n
factor :: Int -> Int -> [Int]
factor n p
    | mod n p == 0          = p:factor (div n p) p
    | mod n p /= 0 && n > 1 = [n]
    | otherwise             = []

main = do
    args <- getArgs
    let n = read $ head args :: Int
    let r = trialDiv n

    putStrLn $ "The largest factor for " ++ (show n) ++ " is " ++ (show $ last r)
