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

--3.Define map and filter
mymap :: (a->b)->[a]->[b]
mymap f xs=myfoldr op [] xs
    where op x ys=f x:ys

{--
map :: (a->b)->[a]->[b]
map f []     = []
map f (x:xs) = (f x:map f xs)

Vad är operatorn?
formen ska bli op x (f xs)
op x y=(x:y)

map (+1) [1,2]=foldr op [] [1,2]=op 1 (foldr op [] 2)=op 1 (op 2 ([])=

Patrik: Skriv upp ekvationerna:
(a):  map f (x:xs) = f x : map f xs             -- Def. map
(b):  foldr op e (x:xs) = op x (foldr op e xs)  -- Def. foldr
(c):  map f xs = foldr op [] xs                 -- Önskning

Vi kan nu kombinera dem:
  map f (x:xs)
= { (a) }
  f x : map f xs
= { (c) }
  f x : foldr op [] xs

men också

  map f (x:xs)
= { (c) }
  foldr op [] (x:xs)
= { (b) }
  op x (foldr op e xs)

Det betyder att
  op x (foldr op e xs)
=
  f x : foldr op [] xs

(Vi vet redan att e = []).
Låt oss kalla (foldr op e xs) för ys. Då har vi:
  op x ys = f x : ys
vilket vi kan använda som definition av funktionen op.
--}
