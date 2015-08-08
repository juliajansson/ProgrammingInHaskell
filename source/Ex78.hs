--1.Show how the list comprehesion [f x|x<-xs,p x] can be defined using map and filter
{--
map f (filter p xs)
map (+1) (filter even [2,3,4])=[3,5]
[(+1) x|x<-[2,3,4],even x]=[3,5]
--}
--2. Define all even, any odd, takeWhile, dropWhile
myalleven :: [Int]->Bool
myalleven xs = filter even xs==xs

myanyodd :: [Int]->Bool
myanyodd xs|filter odd xs==[] =False
           |otherwise         =True

mytakewhile :: (a->Bool)->[a]->[a]
mytakewhile a []                 = []
mytakewhile a (b:bs) | a b       = b:mytakewhile a bs
                     | otherwise = []

mydropwhile :: (a->Bool)->[a]->[a]
mydropwhile a []                 = []
mydropwhile a (b:bs) | a b       = mydropwhile a bs
                     | otherwise = b:bs
