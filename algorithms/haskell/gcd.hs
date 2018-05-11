module GCD
( euclid,
) where

{-
 - Computes gcd by Euclid's algorithm
 - Input: Two nonnegative, not-both-zero integers m and n
 - Output: Greatest common divisor of m and n
 -}
euclid :: Integer -> Integer -> Integer
euclid m n 
    | n == 0    = m
    | m > n     = euclid' (m-n) n
    | otherwise = euclid' m (n-m)

euclid' :: Integer -> Integer -> Integer
euclid' m n
    | n == 0    = m
    | otherwise = euclid n (m `mod` n)
