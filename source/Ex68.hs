import Data.Ratio -- to enable use of (%) for rationals: 1%2 for ½ (half) etc.

--1. Define the operator ^ for nonnegative integers using the same pattern of recursion as the multiplication operator *, and show how 2³ is evaluated using your definition

raise :: Int->Int->Rational
raise 0 y | y==0      = error "What is g0ing 0n"
          | otherwise = 0
raise x 0 = 1
raise x y | y==1      = fromIntegral x
          | y<0       = (1/fromIntegral x)*(raise x (y+1))
          | otherwise =    fromIntegral x *(raise x (y-1))

{--
2.
length [1,2,3] = 1+length [2,3] = 1+1+length [3] = 1+1+1+length [] = 3
drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5] = drop 1 [3,4,5] = drop 0 [3,4,5] = [3,4,5]
init [1,2,3] = (1:init [2,3]) = (1:2:init[3]) = (1:2:[]) = [1,2]
--}
--3. Define and, concat, replicate, (!!), elem
myand :: [Bool]->Bool
myand []        = True
myand (False:_) = False
--myand (True:xs) |myand xs  =True
--                |otherwise =False
myand (True:xs) = myand xs

--myconcat:: [[a]]->[a]
--myconcat [[]]=[]
--myconcat ((x:xs):xss)=(x:xs:(myconcat xss))
{--
myconcat [[4,6,7],[4],[5,6,7])=4:[6,7]:myconcat [[4],[5,6,7]]=4:[6,7]:4:[]:myconcat [[5,6,7]]=4:[6,7]:4:[]:5:[6,7]
--}

myreplicate :: Int->a->[a]
myreplicate 0 _ = []
myreplicate 1 x = [x]
myreplicate n x = (x:myreplicate (n-1) x)
--myreplicate 3 a =(a:myreplicate 2 a)=(a:(a:myreplicate 1 a))=(a:(a:(a)))=[a,a,a]
exclamationmark :: [a]->Int->a
exclamationmark (x:_)  0 = x
exclamationmark (x:xs) n | (n-1)>length (x:xs) = error "There are not enough elements!!!!!!!!!"
                         | n<0                 = error "Which is the negative element of this list???"
                         | otherwise           = exclamationmark xs (n-1)

myelem :: Eq a=>a->[a]->Bool
myelem a (x:xs) | a==x          = True
                | myelem a (xs) = True
                | otherwise     = False

--3. Define merge
merge :: Ord a=>[a]->[a]->[a]
merge [] xs = xs
merge xs [] = xs
merge (y:ys) (x:xs) | y<=x       = y:merge ys (x:xs)
                    | otherwise  = x:merge (y:ys) xs

--4. Define msort and halve

msort:: Ord a => [a]->[a]
msort []  = []
msort [x] = [x]
--msort xs =halvemerge (halve xs)
msort xs = let (as,bs) = halve xs
               sas     = msort as
               sbs     = msort bs
           in merge sas sbs
{--
msort [3,2,1,0] = merge sas sbs = merge (msort [3,2]) (msort [1,0])
= merge (merge [3] [2]) (merge [1] [0])
= merge [2,3] [0,1] = 0:merge [2,3] [1] = 0:1:[2,3] = [0,1,2,3]
--}

halve :: [a]->([a],[a])
halve as | even (length as) = splitAt (div (length as) 2) as
         | otherwise        = splitAt (div ((length as)-1)2) as
-- TODO: can be simplified (uses integer division anyway)

--5. Recurively define sum, take and last
mysum :: Num a=>[a]->a
mysum [] = 0
mysum [x] = x
mysum (x:xs) = x+(mysum xs)

mytake :: Int->[a]->[a]
mytake 0 xs   = xs
mytake _ []   = []
mytake a (b:bs) | a==1      = [b]
                | a<0       = error "Can't take a negative amount of numbers!!"
                | otherwise = (b:mytake (a-1) bs)

mylast :: [a]->a
mylast []     = error "There is no last element, there is no first element, there is no element at all :O"
mylast [x]    = x
mylast (x:xs) = mylast xs
