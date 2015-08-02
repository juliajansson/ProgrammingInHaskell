import Data.Char (ord, chr)
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
factors :: Int -> [Int]
factors n = [x|x<-[1..n],mod n x==0]

perfects :: Int -> [Int]
--the factors that matter: b x =take (length (factors x)-1) (factors x)
--x is a perfect number if sum b==x
perfects a = [x|x<-[1..a], x==sum (take (length (factors x)-1) (factors x))]
--(take ((length xs)-1) xs==init xs

--5. show how the single comprehension with two generators can be redefined with two comprehensions with single generators: [(x,y)|x<-[1,2,3],y<-[4,5,6]]
--Result [(x,y)|x<-[1,2,3],y<-[4,5,6]]=[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
pairing :: [a] ->[b]->[(a,b)]
pairing xs ys = concat [[(x,y)|y<-ys]|x<-xs]
par::[a] ->[b]->[(a,b)]
par xs ys = [(x,y)|x<-xs,y<-ys]

--6. Redefine the function positions using the function find
positions:: Eq a => a->[a]->[Int]
positions b bs= find b (makepairs bs)

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v|(k',v)<-t,k==k']

makepairs:: [a]->[(a,Int)]
makepairs cs = zip cs [1..]

--7.scalarproduct [1,2,3][4,5,6]=32
scalarproduct:: [Int]->[Int]->Int
scalarproduct xs ys = sum [x*y|(x,y)<- zip xs ys]
--chisqr os es = sum [((o-e)^2)/e|(o,e)<-zip os es]

--8. Redefine the Ceasar cipher to handle upper case letters
encode:: Int -> String -> String
encode n xs = [shift n x| x <- xs]
shift :: Int -> Char -> Char
shift n c | isLower c = int2let (mod(let2int c+n)26)
          | isUpper c = int2Let (mod(let2int c+n)26)
                        -- Bug: should subtract position of 'A'. let2int 'A' is -32
          | otherwise = c
int2let:: Int -> Char
int2let n = chr (ord 'a'+n)
int2Let:: Int -> Char
int2Let n = chr (ord 'A'+n)
let2int:: Char -> Int
let2int c = ord c - ord 'a'
-- Another function is missing here
isLower:: Char -> Bool
isLower c = c >= 'a' && c <= 'z'
isUpper:: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'
