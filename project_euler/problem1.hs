--
-- Project Euler
-- Problem 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we
-- get 3,5,6, and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
import System.Environment
import Data.List

get_multiples :: Integer -> [Integer]
get_multiples y = [x | x <- [3,4..y-1], mod x 3 == 0 || mod x 5 == 0]

sum_list :: [Integer] -> Integer
sum_list multiples = sum multiples

main = do
     putStrLn "Sum of multiples 3 or 5 below 1000 is "
     putStrLn (show (sum_list (get_multiples 1000)))
