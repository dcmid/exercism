module DNA (toRNA) where

toRNA :: [Char] -> [Char]
toRNA [] = []
toRNA "A" = "U"
toRNA "C" = "G"
toRNA "G" = "C"
toRNA "T" = "A"
toRNA x = toRNA(head x:[])++toRNA(tail x)