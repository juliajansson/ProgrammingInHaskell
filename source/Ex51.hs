-- List comprehensions - generators and guards, tried out the examples
myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]

mylength :: [a] -> Int
mylength xs = sum [1 | _ <- xs]

myfirsts :: [(a,b)] ->[a]
myfirsts ps = [x | (x,_) <- ps]

myfactors :: Int -> [Int]
myfactors n = [x | x<-[1..n],mod n x==0]
