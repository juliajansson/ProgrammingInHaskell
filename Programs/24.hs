double:: Int -> Int
double x = x+x

quadruple:: Int -> Int
quadruple x = double (double x)

factorial:: Int -> Int
factorial n = product [1..n]

average:: [Int] -> Int
average [] = 0
average as = div (sum as) (length as)