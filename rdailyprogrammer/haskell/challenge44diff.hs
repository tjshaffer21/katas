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
 - TODO Improve Miller-Rabin
 -}
import Prelude
import System.Random
--import Control.Parallel

sieve :: Int -> Int -> [Int]
sieve min max = [x | x <- [min..max], odd x, mod x 3 /= 0, mod x 5 /= 0, 
                 mod x 7 /= 0,millerRabin x 1]

-- Naive implementation
naive :: [Int] -> [Int]
naive []       = []
naive (x:xs)   = x:(naive (dropMult x xs))

dropMult :: Int -> [Int] -> [Int]
dropMult n []       = []
dropMult n (x:xs) 
    | n == x        = x:dropMult n xs
    | mod x n == 0  = dropMult n xs
    | otherwise     = x:dropMult n xs

-- Miller-Rabin
-- Input:   n > 3, an odd integer to be tested for primality
-- Intput:  k, a parameter that determines the accuracy of the test.
millerRabin :: Int -> Int -> Bool 
millerRabin n k
    | n < 1          = error "Invalid integer."
    | n > 3 && k > 0 = let a  = (\mi ma -> head $! take ma 
                                (randomRs (mi, ma) (mkStdGen 100))) 2 (n - 2)
                           ds = findDS $! (n-1)
                           ad = (fromIntegral a) ^ fst ds
                           x :: Integer
                           x  = mod ad (fromIntegral n)
                        in if x == 1 || x == (fromIntegral $! (n - 1)) then
                                millerRabin n $! (k - 1)
                           else
                                mrLoop (fromIntegral x) n k (snd ds) 1
    | otherwise = True

-- x, a^d % n
-- n, Test number
-- k, Accuracy
-- s, From 2^d * s
-- r, Inc. var
mrLoop :: Int -> Int -> Int -> Int -> Int -> Bool
mrLoop x n k s r
    | r < s     = let xx = mod (x * x) n
                  in if xx == 1 then
                        False
                     else if xx == (n - 1) then
                        millerRabin n $! (k - 1)
                     else
                        mrLoop xx n k s $! (r + 1)
    | otherwise = False

-- Find values for (d,s)
findDS :: Int -> (Int, Int)
findDS d
    | mod d 2 == 0 = let ds = findDS $! (div d 2) in (fst ds, (snd ds) + 1)
    | otherwise    = (d, 0)

-- n!
factorial :: Int -> Int
factorial 1 = 1
factorial n
    | n < 1     = error "Invalid number."
    | otherwise = n * (factorial $! (n-1))

-- Calculate both sum and length at the same time.
-- (x:xs),  List
-- s,       Sum
-- l,       Length
-- return (sum, length)
sumAndLength :: [Int] -> (Int, Int)
sumAndLength []     = (0,0)
sumAndLength (x:xs) = let r = sumAndLength xs
                          in (fst r + x, snd r + 1)

main = do putStrLn $ "There are " ++ (show $ snd s) ++ " primes between "++
            (show a) ++ " and " ++ (show $ c) ++ " and the sum is " ++
            (show $ fst s) 
          where a = factorial 9--16
                c = a + factorial 8 -- 8
                s = sumAndLength $ sieve a c

-- No profiling libraries for Control.Parallel
{-do a `pseq` c `pseq` (s `pseq` putStrLn $ "There are " ++ 
            (show $ length s) ++ " primes between " ++ (show a) ++ " and " 
            ++ (show $ c) ++ " and the sum is " ++ (show $ sum s))
          where a = factorial 16
                c = a + factorial 10
                s = sieve a (c)-}
