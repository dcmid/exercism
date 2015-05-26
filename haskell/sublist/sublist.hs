module Sublist (Sublist(Equal, Unequal, Sublist, Superlist), sublist) where

data Sublist = Equal | Unequal | Sublist | Superlist deriving (Eq, Show)

sublist :: (Ord a) => [a] -> [a] -> Sublist
sublist x y
	|x == y = Equal
	|length x > length y && x `contains` y = Superlist
	|length y > length x && y `contains` x = Sublist
	|otherwise = Unequal

contains :: (Ord a) => [a] -> [a] -> Bool
contains [] _ = False
contains _ [] = True
contains container value
	|length value > length container = False
	|value == take (length value) container = True
	|otherwise = (tail container) `contains` value
