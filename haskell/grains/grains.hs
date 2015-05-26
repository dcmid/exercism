module Grains (square, total) where

square :: (Integral a) => a -> a
square x = 2^(x-1)

total :: (Integral a) => a
total = sum [square x | x <- [1..64]]