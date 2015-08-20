--8.2
type Parser a = String -> [(a,String)]
--8.3
myreturn:: a->Parser a
myreturn v= \inp -> [(v,inp)]
--return v inp=[(v,inp)]

myfailure:: Parser a
myfailure= \inp->[]
--failure inp=[]

myitem:: Parser Char
myitem = \inp -> case inp of
                 []->[]
                 (x:xs)->[(x,xs)]
{-
item inp|null inp =[]
        |otherwise=[(head inp,tail inp)]
-}

myparse:: Parser a->String ->[(a,String)]
myparse p inp= p inp


