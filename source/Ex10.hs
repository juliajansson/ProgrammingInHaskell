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

data Nat'=Zero | Succ Nat'
--Succ (+1)
nat2int:: Nat'->Int
nat2int Zero    =0
nat2int (Succ n)=1+nat2int n

int2nat:: Int->Nat'
int2nat 0 =Zero
int2nat n =Succ (int2nat (n-1))

addnat::Nat'->Nat'->Nat'
addnat m n=int2nat (nat2int m + nat2int n)

addNat:: Nat'->Nat'->Nat'
addNat Zero n     = n
addNat (Succ m) n =Succ (addNat m n)

data List a = Nil | Cons a (List a)

len::List a->Int
len Nil        =0
len (Cons _ xs)=1+len xs

data Tree = Leaf Int | Node Tree Int Tree

t:: Tree
t = Node(Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs:: Int->Tree->Bool
occurs m (Leaf n)    = m==n
occurs m (Node l n r)= m==n || occurs m l || occurs m r

flatten:: Tree->[Int]
flatten (Leaf n)=[n]
flatten (Node l n r)=flatten l++[n]++flatten r

occurs':: Tree->[Int]
occurs' m (Leaf n) =m==n
occurs' m (Node l n r)
        |m==n      =Tree
        |m<n       =occurs' m l
        |otherwise =occurs' m r

data Tree a   = Leaf a | Node (Tree a )(Tree a)
data Tree a   = Leaf | Node (Tree a) a (Tree a)
data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
data Tree a   = Node a [Tree a]
