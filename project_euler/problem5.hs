{-
 -  Project Euler
 -  Problem 5
 -
 -  2520 is the smallest number that can be divided by each numbers from 1 to 10
 -  without any remainder.
 -
 -  What is the smallest positive number that is evenly divisible by all of the
 -  numbers from 1 to 20?
 -}
import System (getArgs)

-- The numbers to check divisibility
-- TODO Better algorithm to remove unnecessary numbers.
--
-- Algorithm:
--  1.  Even numbers are only divisible by even numbers.
--  2.  Cannot be < n
--  3.  If n >= 10 then only include numbers by increments of 10.
--
-- n - the max of the divisibility check.
getNums :: Int -> [Int]
getNums n = [x | x <- [n..n^9], even x, 
    if n >= 10 then mod x 10 == 0 else  even x]

-- Check the divisibility
--
-- xs - The list of numbers to check.
-- ys - The list of divisibility numbers.
checkDiv :: [Int] -> [Int] -> Int
checkDiv [] ys     = 0 
checkDiv (x:xs) ys
    | checkDiv' x ys == True = x
    | otherwise      = checkDiv xs ys

-- Check if n is divisble by all xs
--
-- n    - Number to check
-- xs   - Divisibility numbers
checkDiv' :: Int -> [Int] -> Bool
checkDiv' n []               = True
checkDiv' n (x:xs)
    | (mod n x == 0) == True = True && checkDiv' n xs
    | otherwise              = False


main=do
    args <- getArgs
    let n = read $ head args :: Int
    let l = getNums n
    let d = [1,2..n]

    let r = checkDiv l d

    putStrLn $ "The smallest positive number that is evenly divisible of" ++
        " all numbers from 1 to " ++ (show n) ++ " is " ++ 
        if r == 0 then " not found " else (show r)
