{- Challenge #44 Difficult
 -
 - Write a function that takes two arguments a and be, and finds all primes that
 - are between a and a + b (specifically, find all primes p such that a <= p < a
 - + b). So for instance, for a = 1234 and b = 100, it should return the
 - following 15 primes:
 -
 - 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319,
 - 1321, 1327
 -
 - The sum of those primes are 19339. The number of primes for a = 9! and b = 8!
 - is 3124 and the sum of those primes is 1196464560.
 -
 - How many primes are there for a = 16! and b = 10!, and what is their sum?
 -
 - TODO Implement more efficent algorithm.
 -}
import System.Random

sieve :: Int -> Int -> [Int]
sieve min max = [x | x <- 2:primesN [y | y <- [3..max], odd y], x >= min]

-- Naive implementation
primesN :: [Int] -> [Int]
primesN []       = []
primesN (x:xs)   = x:(primesN (dropMult x xs))

dropMult :: Int -> [Int] -> [Int]
dropMult n []       = []
dropMult n (x:xs) 
    | n == x        = x:dropMult n xs
    | mod x n == 0  = dropMult n xs
    | otherwise     = x:dropMult n xs

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

main = do
    let a = factorial 16
    let b = factorial 10
    let s = sieve a (a + b)

    putStrLn $ "There are " ++ (show $ length s) ++ " primes between " ++ 
        (show a) ++ " and " ++ (show $ a + b) ++ " and the sum is " ++ 
        (show $ sum s)
