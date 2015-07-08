myproduct :: [Int]->Int
myproduct []     = 1
myproduct (x:xs) = x*myproduct xs
