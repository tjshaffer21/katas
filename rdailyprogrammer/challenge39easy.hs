{--
 -  Challenge #39 Easy
 -  FizzBuzz
 -
 -  You are to write a function that displays the numbers from 1 to an input
 -  parameter n, one per line, except that if the current number is divisible by
 -  3 the function should write "Fizz" instead of the number, if the current
 -  number is divisible by 5 the function should write "Buzz" instead of the
 -  number, and if the current number is divisble by both 3 and 5 the fuction
 -  should write "FizBuzz" instead of the number.
 -  
 -  Pseudocode:
 -  
 -  from [1..n]
 -      if (i % 3 && i % 5) then
 -          print "FizzBuzz"
 -      else if i % 3 then 
 -          print "Fizz"
 -      elif i % 5 then 
 -          print "Buzz"
 -      else
 -          print i
 -      end
 -
 -  To Do:
 -  - Implement is different ways after learning more Haskell.
 -}
import System

fizzbuzz :: [Int] -> IO ()
fizzbuzz xs = mapM_ (\x -> do 
    if mod x 5 == 0 && mod x 3 == 0 then
        print "FizzBuzz" 
    else if mod x 5 == 0 then
        print "Buzz"
    else if mod x 3 == 0 then
        print "Fizz"
    else
        print x) xs

main = do
    args <- getArgs
    fizzbuzz [1..(read $ head args)]
