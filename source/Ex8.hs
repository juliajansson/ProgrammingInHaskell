import Data.Char (ord, chr)
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

--More parse functions
pmap:: (a->b)->Parser a->Parser b
pmap f pa = pb
  where pb s = pa (f s)
--map for parsers, not for lists
--map:: (a->b)->[a]->[b]

char2int :: Char -> Digit
char2int c = ord c - ord '0'

type Digit = Int
digit :: Parser Digit
digit = pmap char2int myitem
