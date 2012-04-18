{-- TODO Allow use of mix case
 -  TODO Use strip and block methods
 -}

module Cipher (caesar, dcaesar, rot13, drot13) where 
import Data.Char (chr, ord, isAlpha, isUpper, isLower)

-- Strip string of punctuation and white space
strip :: String -> String
strip s = [x | x <- map (\y -> if not $ isAlpha y then ' ' else y) s]

-- Split string into fixed length blocks.
block :: String -> Int -> String
block [] n = [] 
block x  n = let t = splitAt n x in fst t ++ [' '] ++ (block (snd t) n)

toUpper :: Int -> Int
toUpper n
    | isUpper $ chr n = n
    | otherwise       = n - 32    -- Difference between lower and upper case

-- Wrap uppercase alphabet
asciiWrapU :: Int -> Int
asciiWrapU n
    | (n > 64 && n <= 90) = n
    | otherwise           = (mod (n - 65) 26) + 65

caesar :: String -> Int -> String
caesar s n = [x | x <- map (\y -> 
    if isAlpha y then chr $ asciiWrapU $ (toUpper $ ord y) + n else y) s]

dcaesar :: String -> Int -> String
dcaesar s n = [x | x <- map (\y -> 
    if isAlpha y then chr $ asciiWrapU $ (toUpper $ ord y) - n else y) s]

rot13 :: String -> String
rot13 s = caesar s 13

drot13 :: String -> String
drot13 s = dcaesar s 13
