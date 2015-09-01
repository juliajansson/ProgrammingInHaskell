--10.1
--type String   =[Char]
type Board    =[Pos]
type Pos      =(Int,Int)
--Not allowed: type Tree   =(Int, Tree)
type Parser a =String->[(a, String)]
--type IO a     =World ->(a, World)
type Assoc k v=[(k,v)]
find:: Eq k =>k->Assoc k v->v
find k t = head [v|(k',v)<-t, k==k']

--10.2
--data Bool =False|True
data Move' = Left'|Right'|Up'|Down'

move::Move'->Pos->Pos
move Left'  (x,y)=(x-1,y)
move Right' (x,y)=(x+1,y)
move Up'    (x,y)=(x,y-1)
move Down'  (x,y)=(x,y+1)

moves::[Move']->Pos->Pos
moves []     p=p
moves (m:ms) p=moves ms (move m p)

flip::Move'->Move'
flip Left' =Right'
flip Right'=Left'
flip Up'   =Down'
flip Down' =Up'

data Shape = Circle Float|Rect Float Float

square :: Float->Shape
square n = Rect n n

area:: Shape->Float
area (Circle r) =pi*r^2
area (Rect x y) =x*y

omkrets::Shape->Float
omkrets (Circle r)=pi*2*r
omkrets (Rect x y)=2*x*y

data Maybe' a=Nothing'|Just' a

safediv:: Int->Int->Maybe' Int
safediv _ 0=Nothing'
safediv m n =Just'(div m n)

safehead::[a]->Maybe' a
safehead []=Nothing'
safehead xs=Just'(head xs)
