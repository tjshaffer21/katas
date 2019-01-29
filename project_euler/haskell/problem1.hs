-- Project Euler
-- Problem 1
import System.Environment
import Data.List

get_multiples :: Integer -> [Integer]
get_multiples y = [x | x <- [3,4..y-1], mod x 3 == 0 || mod x 5 == 0]

sum_list :: [Integer] -> Integer
sum_list multiples = sum multiples

main = do
     putStrLn "Sum of multiples 3 or 5 below 1000 is "
     putStrLn $ show $ sum_list $ get_multiples 1000
