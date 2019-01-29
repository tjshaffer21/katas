-- p06 Find out whether a list can be a palindrome.
-- p06 [x] -> bool

reverse_list :: [a] -> [a]
reverse_list [] = []
reverse_list [x] = x:[]
reverse_list (x:xs) = reverse_list xs ++ [x]

p06 :: Eq a => [a] -> Bool
p06 [] = True
p06 x
    | x == reverse_list x = True
    | otherwise = False

main = do
    putStrLn $ "Result: " ++ (show $ p06 [1,0,0,1])
    putStrLn $ "Result: " ++ (show $ p06 [1,1,0,0])
    putStrLn $ "Result: " ++ (show $ p06 ['a', 'b', 'c', 'd'])
    putStrLn $ "Result: " ++ (show $ p06 "madam")