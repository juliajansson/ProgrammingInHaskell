--higher-order functions
quadruple::(a->a)->a->a
quadruple f x=f(f(f(f x)))

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
