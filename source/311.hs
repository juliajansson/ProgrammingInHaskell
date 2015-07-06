second:: [a] -> a
second xs =head (tail xs)

swap:: (a,b)->(b,a)
swap (x,y) = (y,x)

pair:: a -> b -> (a,b)
pair x y = (x,y)

double:: Num a => a -> a
double x = x*2

{-I need help with defining these types

palindrome:: [a] -> Bool
palindrome xs = reverse xs == xs

twice:: (a -> a )->( a -> a -> a) 
twice f x = f (f x) -}