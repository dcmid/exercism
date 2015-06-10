module SumOfMultiples where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples factorList bound = sum $ filter (multipleOfList factorList) [1 .. bound-1]
  where
    multipleOfList :: [Int] -> Int -> Bool
    multipleOfList [] _ = False
    multipleOfList (x:xs) num = num `mod` x == 0 || multipleOfList xs num

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault = sumOfMultiples [3, 5]