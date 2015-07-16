--1. Calculate the sum of the first 100 integer squares
ex1 = [x^2|x<-[1,2..100]]

--2. Define the funcion replicate with a list comprehension
--replicate 3 True
--[True,True,True]
myreplicate :: Int -> a -> [a]
myreplicate a b = [b|c<-[1..a]]
--length xs= sum [1|_<-xs]

test xss = [(x,xs) | xs <- xss, x <- xs]

testlista = [[1,7],[3],[8,17,38]]

{-
vad blir test testlista?
  test xss
=
  [(x,xs) | xs <- testlista, x <- xs]
= xs blir en i taget från testlista: 
      [(x,[1,7])        | x <- [1,7]]
  ++  [(x,[3])          | x <- [3]]
  ++  [(x,[8, 17, 38])  | x <- [8, 17, 38]]
= x blir en i taget från [1,7] osv.
      [(1,[1,7]), (7,[1,7])]
  ++  [(3,[3])]
  ++  [(8, [8, 17, 38]), (17,[8, 17, 38]), (38, [8, 17, 38])]
= 
  [(1,[1,7]), (7,[1,7]), (3,[3]), (8, [8, 17, 38]), (17,[8, 17, 38]), (38, [8, 17, 38])]


  [x | xs <- testlista, x <- xs]
=
  [1, 7, 3, 8, 17, 38]
-}

{-
  [(x,xs) | xs <- testlista, x <- xs]
=
  concatMap (\xs -> [(x,xs) | x <- xs]) [[1,7],[3],[8,17,38]]
=
  concat [ [(x,[1,7]) | x <- [1,7]],
           [(x,[3]) | x <- [3]],
           [(x,[8,17,38]) | x <- [8,17,38]] ]
= 
           [(x,[1,7]) | x <- [1,7]],
        ++ [(x,[3]) | x <- [3]],
        ++ [(x,[8,17,38]) | x <- [8,17,38]] ]
-}

--3. pyths 10
--   [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths:: Int -> [(Int,Int,Int)]
pyths a = [(x,y,z)|x<-[1..a],y<-[1..a],z<-[1..a],x^2+y^2==z^2]

--4. perfects 500
--   [6,28,496]
perfects :: Int -> [Int]
--the factors that matter: b x =take (length (factors x)-1) (factors x)
--x is a perfect number if sum b==x
