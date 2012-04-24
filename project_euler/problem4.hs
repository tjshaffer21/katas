{-
 - Project Euler
 - Problem 4
 - A palindromic number reads the same both ways. The largest palindrome made
 - from the product of two 2-digit numbers is 9009 = 91 x 99.
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 -}
import System (getArgs)

-- Create a list of all numbers with n-digits
createNumList :: Int -> [Int]
createNumList n = let y = 10^(n-1) in [x | x <- [y, y+1..(10^n)-1]]

-- Find the
findProducts :: [Int] -> [Int] -> [Int]
findProducts [] []          = []
findProducts []  x          = x
findProducts y  []          = y
findProducts (y:ys) (x:xs)  = findProducts' y (x:xs) ++ findProducts ys (x:xs) 

-- Find the products of n*[]
findProducts' :: Int -> [Int] -> [Int]
findProducts' 0 _        = [0]
findProducts' _ []       = []
findProducts' n (x:xs)   = (n*x):findProducts' n xs

-- Find the largest palindrome
findPalindrome :: [Int] -> Int
findPalindrome []     = 0 
findPalindrome (x:xs) = let y = map (\z -> read [z] :: Int) (show x)
                        in if y == reverse y then 
                                max (findPalindrome xs) x 
                           else 
                                findPalindrome xs

main = do
    args <- getArgs
    let n    = read $ head args :: Int
    let list = createNumList n
    let r    = findPalindrome $ findProducts list (reverse list)

    putStrLn $ "The largest palindrome of two " ++ (show n) ++ 
        " digit numbers is " ++ (show r)


