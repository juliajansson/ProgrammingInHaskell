--Recursive functions!!! YAY!
myeven :: Int -> Bool
myeven 0 = True
myeven n = myodd (n-1)

myodd :: Int -> Bool
myodd 0 = False
myodd n = myeven (n-1)

evens :: [a]->[a]
evens []     = []
evens (x:xs) = x:odds xs

odds :: [a]->[a]
odds []     = []
odds (_:xs) = evens xs

myzip :: [a]->[b]->[(a,b)]
myzip _[]=[]
myzip []_=[]
myzip (x:xs) (y:ys)=(x,y):myzip xs ys

mydrop :: Int ->[a]->[a]
mydrop 0 xs = xs
mydrop _ [] = []
mydrop n xs = mydrop (n-1) xs

isort :: Ord a=>[a]->[a]
isort []     = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a->[a]->[a]
insert x []                = [x]
insert x (y:ys) |x<=y      = x:y:ys
                |otherwise = y:insert x ys

glue :: [a]->[a]->[a]
glue []     ys = ys
glue (x:xs) ys = x:glue xs ys

myreverse :: [a]->[a]
myreverse []     = []
myreverse (x:xs) = glue (myreverse xs) [x]

mylength :: [a]->Int
mylength []     = 0
mylength (_:xs) = 1+mylength xs

add :: (Eq a, Integral a)=> a -> a ->a
add m 0       = m
add m n | n>0 = add (m+1) (n-1)
        | n<0 = add (m-1) (n+1)

multiply :: (Eq a, Integral a)=> a->a->a
multiply m 0       = 0
multiply m n | n>0 = add m    (multiply m (n-1))
             | n<0 = add (-m) (multiply m (n+1))

myinit :: [a]->[a]
myinit (x:xs) | null xs   = []
              | otherwise = x:myinit xs

-- myinit [5,6,7]
--(5:myinit [6,7])
--(5:6:myinit[7])
--(5:6:[])
--[5,6]
