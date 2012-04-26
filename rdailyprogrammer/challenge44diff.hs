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
import Control.Parallel

sieve :: Int -> Int -> [Int]
sieve min max = [x | x <- [min..max], odd x, millerRabin x 1]

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
    | n < 0          = error "Invalid integer."
    | n < 3 && n > 1 = True
    | k > 0     = let a = head . head $ take 1 (rand (mkStdGen 100) 2 (n - 2))
                      ds = findDS (n-1)
                      d  = fst ds
                      x :: Integer
                      x  = mod ((fromIntegral a) ^ d) (fromIntegral n) 
                  in if x == 1 || x == ((fromIntegral n) - 1) then
                        millerRabin n (k - 1)
                     else
                        mrLoop (fromIntegral x) n k ((snd ds)-1)
    | otherwise = True

-- x, a^d % n
-- n, Test number
-- k, Accuracy
-- s, From 2^d * s
mrLoop :: Int -> Int -> Int -> Int -> Bool
mrLoop x n k s
    | s > 0     = let xx = mod (x * x) n
                  in if xx == 1 then
                        False
                     else if xx == (n - 1) then
                        millerRabin n (k - 1)
                     else
                        mrLoop xx n k (s - 1)
    | otherwise = False

-- Find values for (d,s)
findDS :: Int -> (Int, Int)
findDS d
    | mod d 2 == 0 = let ds = findDS (div d 2) in (fst ds,(snd ds) + 1)
    | otherwise    = (d, 0)

-- Produce random numbers [min,max]
rand :: RandomGen a => a -> Int -> Int -> [[Int]]
rand g min max = randomW (randomRs (min, max) g) max

randomW :: [Int] -> Int -> [[Int]]
randomW (r:rs) max = let (xs,ys) = splitAt max rs in xs : randomW ys max

factorial :: Int -> Int
factorial 1 = 1
factorial n
    | () `seq` n `seq` False = undefined
    | n < 1     = error "Invalid number."
    | otherwise = n * factorial (n-1)

main = do a `pseq` c `pseq` (s `pseq` putStrLn $ "There are " ++ 
            (show $ length s) ++ " primes between " ++ (show a) ++ " and " 
            ++ (show $ c) ++ " and the sum is " ++ (show $ sum s))
          where a = factorial 9--factorial $! 16
                c = a + factorial 8--factorial $! 10
                s = sieve a (c)
