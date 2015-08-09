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
myfoldr :: (a->b->b)->b->[a]->b
myfoldr f v []     = v
myfoldr f v (x:xs) = f x (myfoldr f v xs)


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

myfilter:: (a->Bool)->[a]->[a]
myfilter p xs= myfoldr op [] xs
   where op y ys | p y       = y:ys
                 | otherwise = ys

{-
(a): filter p (x:xs)|p x       = x:filter p xs
                    |otherwise = filter p xs   --(Definition av filter)
(b): foldr op e (x:xs) = op x (foldr op e xs) --(Definition av foldr)
(c): filter p xs       = foldr op [] xs       --(Önskning)

  op x (foldr op e xs)
= { (b) "baklänges" }
  foldr op e (x:xs)
= { (c) }
  filter p (x:xs)
= { (a) }
  if p x  then x:filter p xs
          else   filter p xs
= { (c) }
  if p x  then x:foldr op [] xs
          else   foldr op [] xs

Generallisera från (foldr op e xs) till ys
  op x ys = if p x then x:ys else ys
Det blir definitionen av op!


{- Some confusion
filter p (y:x:xs)|p y       =y:filter p (x:xs)
                 |otherwise =filter p (x:xs)
=
filter p (y:x:xs)|p y       =y:foldr op [] (x:xs)
                 |otherwise =foldr op [] (x:xs)
=
filter p (y:x:xs)|p y       =y:op x (foldr op [] xs)
                 |otherwise =op x (foldr op [] xs)

Men också:
filter p (y:x:xs)
=
foldr op e [] (y:x:xs)
=
op y (foldr op [] (x:xs))

Det betyder att:
op y (foldr op [] (x:xs))|p y       =y:op x (foldr op [] xs)
                         |otherwise =op x (foldr op [] xs)
-}



-}
