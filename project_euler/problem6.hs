{-  Project Euler
 -  Problem 6
 -
 -  The sum of squares of the first ten natural numbers is,
 -      1^2 + 2^2 + ... + 10^2 = 385
 -
 -  The square of the sum of the first ten natural numbers is,
 -      (1+2+ ... + 10)^2 = 55^2 = 3025
 -
 -  Hence the difference between the sum of the squares of the first ten natural
 -  numbers and the square of the sum is 3025 - 385 = 2640.
 -  
 -  Find the difference between the sum of the squares of the first one hundred
 -  natural numbers and the square of the sum.
 -}
import System (getArgs)

main=do
    args <- getArgs
    let max  = read $ head args
    let list = [1..max]

    let diff = ((sum list) ^ 2) - sum [x*x | x <- list]
    putStrLn $ "The difference of the sum of the squares of the first " 
        ++ (show max) ++ " natural numbers and the square of the sum is " 
        ++ (show diff)
