-- p04 Find the number of elements of a list.
-- p04 [a] -> n

-- p04 [1..10] = 10
p04 :: [a] -> Int
p04 [] = 0
p04 [x] = 1
p04 (x:xs) = 1 + p04 xs

-- p04 [1..10] = 10
main = do
    putStrLn $ "Results: " ++ (show $ p04 [1..10])