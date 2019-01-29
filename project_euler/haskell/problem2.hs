-- Project Euler
-- Problem 2
import Debug.Trace

sq5 = sqrt $ (fromIntegral 5 :: Double)
phi = (1.0 + sq5) / (fromRational 2.0)
psi = (1.0 - sq5) / (fromRational 2.0)

-- FIX: Integer overflow when N > 57
binet :: Int -> Int
binet k = let
        sqf = (fromIntegral 1 :: Double) / sq5
        phik = phi ** (fromIntegral k :: Double)
        psik = psi ** (fromIntegral k :: Double)
    in round $ (sqf * (phik - psik))

sums :: Int -> Int -> Int -> Int
sums start limit term = let
    bv = binet start
    in if bv >= limit then 0
       else bv + sums (start + term) limit term

main = do
    putStrLn $ "Sum of the even-valued terms of the fibonacci series no greater"
             ++ " than four million is: " ++ (show $ sums 3 4000000 3)
             -- Every third term is even so start at 3 and skip by 3.