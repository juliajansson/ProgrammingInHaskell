import Data.Char (ord, chr)
--1. Define the parser function int.
type Parser a=String -> [(a,String)]
-- int:: Parser Int
-- int :: String -> [(Int,String)]
int :: [Char] -> [(Int,String)]
int []       = []
int ('-':cs) = change_sign (nat cs)
-- parse cs as a positive number, then change sign
int (c:cs)   =nat (c:cs)
                            
type Nat = Int
nat :: Parser Int
nat []     = []
{-
nat (c:cs) | '0' <= c && c <= '9' = [((op ((char2int c):nat cs)), rest)]
           |otherwise             = [((op (nat cs)),rest)]
-}
nat cs =[(op (map char2int(takeWhile isdig cs)),dropWhile isdig cs)]
           

isdig::Char->Bool
isdig c='0' <= c && c <= '9'

rest (c:cs)|isdig c  =rest cs
           |otherwise=(c:cs)

op::[Int]->Int
op []=0
op (x:xs)=(10^(a))*x+op xs
      where a = length xs

type Digit = Int -- between 0 and 9
int2char :: Digit -> Char
int2char d = chr (ord '0' + d)

char2int :: Char -> Digit
char2int c = ord c - ord '0'

change_sign :: [(Int,String)] -> [(Int,String)]
change_sign [(d,ds)]=[((-d),ds)]


--inp==[Int]=[(v,inp)]
--inp=="-"++[Int]=[("-",Int)]
