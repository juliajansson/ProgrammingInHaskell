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
-}

myfilter:: (a->Bool)->[a]->[a]
myfilter p xs= myfoldr op [] xs
    where op y ys | p y       = y:ys
                  | otherwise = ys
{-
where op y (myfoldr op [] (x:xs))|p y       =(y:op x (myfoldr op [] xs))
                                 |otherwise =(op x (myfoldr op [] xs))
myfoldr op [] xs=kan förenklas till=xs
Test:
myfilter even []=myfoldr op [] []=[]
myfilter even [3,4]=myfoldr op [] [3,4]=[4] 
-}
{-
(a): filter p (x:xs)|p x       =x:filter p xs
                    |otherwise =filter p xs   --(Definition av filter)
(b): foldr op e (x:xs) = op x (foldr op e xs) --(Definition av foldr)
(c): filter p xs       = foldr op [] xs       --(Önskning)

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

--4. Define dec2int using foldl
{-
dec2int [2,3,4,5]=2345
f v []=v
f v (x:xs) = f (v op x) xs

foldl op v []=v
foldl op v (x:xs)=foldl (op v x) xs

dec2int' v []=v
dec2int' v (x:xs)=dec2int' (v op x) xs

dec2int' 0 []=0
dec2int' 0 (x:xs)=dec2int' (0 op x) xs

dec2int' 0 [2,3,4,5]=dec2int' (0 op 2) [3,4,5]=dec2int' ((0 op 2) op 3) [4,5]=
dec2int' (((0 op 2) op 3) op 4) [5]=dec2int' ((((0 op 2) op 3) op 4) op 5) []=
(op (op (op (op 0 2) 3) 4) 5)=(vill)=2345=2*1000+3*100+4*10+5*1

op 0 2=2
op (op 0 2) 3 =23
op 2 3=23
op (op (op 0 2) 3) 4=234
op 23 4=234
op (op (op (op 0 2) 3) 4) 5=2345
op 234 5=2345

op x y=10x+y

dec2int:: [Int]->Int
dec2int=dec2int' 0
     where
        dec2int' v []=v
        dec2int' v (x:xs)= dec2int' (op v x) xs
-}
myfoldl::(a->b->a)->a->[b]->a
myfoldl f v []=v
myfoldl f v (x:xs)=myfoldl f (f v x) xs

dec2int:: [Int]->Int
--dec2int xs= myfoldr op 0 xs
dec2int xs=foldl op 0 xs
   where op x y = 10*x + y

{-
Test, när op är fel
dec2int [8]=foldr op 0 [8]= op 8 (foldr op 0 [])= op 8 0=80
dec2int [8,8]=foldr op 0 (8:[8])= op 8 (foldr op 0 [8])=op 8 (op 8 0)=op 8 80=160
Test, när op är tvärtom
dec2int [9,8]= foldr op 0 [9,8]=op 9 (op 8 0)=op 9 8=op 89

Test, med ny op
dec2int [9,8]= op 9 (op 8 0)=op 9 8=98
dec2int [8,8,8,9]=op 8(op 8(op8(op 9 0)))=op 8( op 8 89)=
-}

--5. Explain why is sumsqreven = compose [sum, map (^2),filter even] is invalid
{-
Definitioner:
sumsqreven ns = sum (map (^2) (filter even ns))
sumsqreven = sum . map (^2) . filter even

Varför funkar inte denna då?
sumsqreven = compose [sum, map (^2),filter even]

Definition av compose:
compose:: [a->a]->(a->a)
compose = foldr (.) id
compose []=id
compose (f:fs)= f . compose fs

compose [sum,map (^2), filter even]=sum . compose [map (^2), filter even]=
sum . map(^2) . filter even . id=sum (map (^2) (filter even (id)))

Test:
sum (map (^2) (filter even (id ns)))=sum (map (^2) (filter even ns))

It's working?
-}
