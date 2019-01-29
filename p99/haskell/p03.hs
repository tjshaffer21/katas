-- p03 Find the K'th element of a list.
-- The first element in the list is number 1.
-- p03 [a] k -> n

import Data.Maybe

-- p03 [1..10] 5 -> 5
p03 :: [a] -> Int -> Maybe a
p03 [] k = Nothing
p03 (x:xs) k
    | (k-1) < 0 = Nothing
    | (k-1) == 0 = Just x
    | (k-1) > 0 = p03 xs (k-1)

main = do
    putStrLn $ "Result: " ++ (show $ fromJust $ p03 [1..10] 5)
    putStrLn $ "Result: " ++ (show $ fromJust $ p03 [1..10] 10)
    putStrLn $ "Result: " ++ (show $ fromJust $ p03 [1..10] 1)