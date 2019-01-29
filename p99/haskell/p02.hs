-- p02 Find the last but one element of a list
-- p02 [x] -> e, e is the last but one element of the list.

import Data.Maybe

-- p02 [1..4] -> 3
p02 :: [a] -> Maybe a
p02 [] = Nothing
p02 [x] = Just x
p02 [x,y] = Just x
p02 (_:xs) = p02 xs

-- p02' [1..4] -> 3
-- Version calls length x then calls p02''
p02' :: [a] -> Maybe a
p02' [] = Nothing
p02' x = p02'' x $ length x

-- p02'' [1..4] 4 -> 3
-- Version requires length to be known.
p02'' :: [a] -> Int -> Maybe a
p02'' [] n = Nothing
p02'' (x:xs) n
    | n == 2    = Just x
    | otherwise = p02'' xs (n - 1)

main = do
    let vals = [1..8]
    let r1 = p02 vals
    let r2 = p02' vals
    let r3 = p02'' vals 8

    putStrLn $ "Result: "
    print $ fromJust $ p02 vals
    print $ fromJust $ p02' vals
    print $ fromJust $ p02'' vals 8