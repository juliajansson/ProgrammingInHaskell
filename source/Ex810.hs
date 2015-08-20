--1. Define the parser function int.
type Parser a=String -> [(a,String)]
int:: Parser Int
int v inp |null inp=[]
          |inp==[Int]=[(v,inp)]
          |inp=="-"++[Int]=[("-",Int)]
