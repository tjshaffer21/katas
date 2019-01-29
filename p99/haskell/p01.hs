-- p01 Find the last element of a list.
-- p01 [x] -> e, e is the last element of the list.
-- p01 [1,2,3,4] -> 4

import Data.Maybe

p01 :: [a] -> Maybe a
p01 [] = Nothing
p01 [x] = Just x
p01 (_:xs) = p01 xs

main=do
    putStrLn $ "Result: " ++ (show $ fromJust $ p01 [1..4])