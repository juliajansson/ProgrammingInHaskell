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
{-
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
-}
--8.4
mysequencing::Parser a->(a->Parser b)->Parser b
mysequencing p f=(\inp-> case myparse p inp of
                       []->[]
                       [(v,out)]->myparse (f v) out)
{-
p:: Parser (Char, Char)
p = do x <- myitem
       myitem
       y <- myitem
       return (x,y)

orelse:: Parser a->Parser a->Parser a
orelse p q=\inp->case parse p inp of
                      []->parse q inp
                      [(v, out)]->[(v, out)]

sat:: (Char->Bool)->Parser Char
sat p = do x <- myitem
           if p x then return x else myfailure
-}
--Nothing works.
