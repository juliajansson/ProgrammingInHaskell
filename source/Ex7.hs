--7.1 higher-order functions
quadruple::(a->a)->a->a
quadruple f x=f(f(f(f x)))

--7.2 map and filter
map1:: (a->b)->[a]->[b]
map1 f xs = [f x|x<-xs]

map2:: (a->b)->[a]->[b]
map2 f []=[]
map2 f (x:xs) =f x:map f xs

filter1:: (a->Bool)->[a]->[a]
filter1 p xs = [x|x<-xs,p x]

filter2:: (a->Bool)->[a]->[a]
filter2 p []               =[]
filter2 p (x:xs)|p x       =(x:filter2 p xs)
                |otherwise =filter2 p xs

sumsqreven:: [Int]->Int
sumsqreven xs=sum(map (^2)(filter even xs))

--7.3 foldr
myfoldr::(a->b->b)->b->[a]->b
myfoldr f v []    =v
myfoldr f v (x:xs)=f x (myfoldr f v xs)

{--
sum    =foldr (+) 0
product=foldr (*) 1
or     =foldr (||) False
and    =foldr (&&) True
--}

myreverse:: [a]->[a]
myreverse xs =myfoldr klistrasist [] xs

klistrasist:: a->[a]->[a]
klistrasist x []=[x]
klistrasist x xs=xs ++ [x]

{--
klistra::[a]->(a->[a]->[a])
klistra ys=foldr (:) ys
--}

mylength:: [a]->Int
mylength xs =myfoldr (\_ n -> 1+n) 0 xs

{--
mymap:: (a->b)->[a]->[b]
mymap f xs=myfoldr op [] xs
--}

{--
map:: (a->b)->[a]->[b]
map f []=[]
map f (x:xs)=(f x:map f xs)

Vad är operatorn?
formen ska bli op x (f xs)
op x y=(x:y)

map (+1) [1,2]=foldr op [] [1,2]=op 1 (foldr op [] 2)=op 1 (op 2 ([])=

--}

myconcat:: [[a]]->[a]
myconcat xs =myfoldr (++) [] xs

{--
myconcat [[81],[9,0]]=myfoldr (++) [] xs= myfoldr (++) [] (x:xs)=
x ++ (myfoldr (++) [] xs)=[81]++[9,0]=[81,9,0]
--}
