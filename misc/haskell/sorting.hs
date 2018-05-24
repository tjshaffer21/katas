module Sorting (bubbleSort, mergeSort) where

import System.Random
import Data.List

-- Bubble Sort
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort x = bubbleSortLoop (length x) bubbleSortWork x

bubbleSortLoop :: (Num a, Ord a) => a -> (t -> t) -> t -> t
bubbleSortLoop n f x
    | n > 0     = bubbleSortLoop (n-1) f x'
    | otherwise = x
    where x' = f $! x 

bubbleSortWork :: (Ord a) => [a] -> [a]
bubbleSortWork []      = []
bubbleSortWork (x:[])  = [x]
bubbleSortWork (x:y:xs)
    | x > y        = y:bubbleSortWork (x:xs)
    | otherwise    = x:bubbleSortWork (y:xs)


-- Merge Sort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []     = error "List is empty"
mergeSort (x:[]) = [x]
mergeSort x      = merge s1 s2
    where
        sList = splitAt (length x `div` 2) x
        s1    = mergeSort (fst sList)
        s2    = mergeSort (snd sList)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] []      = error "Lists are empty"
merge [] ys      = ys
merge xs []      = xs
merge (x:xs) (y:ys)
    | (length xs)+1 > 0 && (length ys)+1 > 0 =
        let result = if x <=  y
                        then 
                            x:merge xs (y:ys)
                        else 
                            y:merge (x:xs) ys
        in result
    | (length xs)+1 > 0 = let result = x:[]
                          in result ++ merge xs (y:ys)
    | (length ys)+1 > 0 = let result = y:[] 
                          in result ++ merge (x:xs) ys

main=do
    let rs = [38,27,43,3,9,82,10,-6,0,-43,100]
    
    let sort = bubbleSort rs  
    --let sort = mergeSort rs
    putStr $ unlines $ map show (sort)
