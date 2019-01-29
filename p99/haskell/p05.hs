-- p05 Reverse a list
-- p05 [a] -> [a]

p05 :: [a] -> [a]
p05 [] = []
p05 [x] = x:[]
p05 (x:xs) = p05 xs ++ [x]

main = do
    putStrLn $ "Results: " ++ (show $ p05 [1..10])