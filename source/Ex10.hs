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

data Nat'=Zero | Succ Nat' deriving Show
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

data Tree = Leaf Int | Node Tree Int Tree deriving Show

t:: Tree
t = Node(Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs:: Int->Tree->Bool
occurs m (Leaf n)    = m==n
occurs m (Node l n r)= m==n || occurs m l || occurs m r

flatten:: Tree->[Int]
flatten (Leaf n)=[n]
flatten (Node l n r)=flatten l++[n]++flatten r

occurs':: Int->Tree->Bool
occurs' m (Leaf n) =m==n
occurs' m (Node l n r)
        |m==n      =True
        |m<n       =occurs' m l
        |otherwise =occurs' m r

                    
data Tree1 a   = Leaf1 a | Node1 (Tree1 a )(Tree1 a)
data Tree2 a   = Leaf2 | Node2 (Tree2 a) a (Tree2 a)
data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)
data Tree4 a   = Node4 a [Tree4 a]

instance Num Nat' where
  (+)=addNat
  (-)=subNat
  (*)=mult
  fromInteger = intToNat

intToNat::Integer->Nat'
intToNat 0=Zero
intToNat a|a>0=Succ (intToNat (a-1))
          |a<0=error "NAJJJJJJ"

natToInt::Nat'->Integer
natToInt Zero=0
natToInt (Succ a)=1+ natToInt a
--Exercise 1, using recursion and add define mult
mult:: Nat'->Nat'->Nat'
mult Zero b=Zero
mult a b   = addNat (mult (a-1) b) b

fabmult::Nat'->Nat'->Integer
fabmult a b=natToInt (mult a b)

subNat:: Nat'->Nat'->Nat'
subNat a Zero=a
subNat (Succ a) (Succ b)= subNat a b
subNat Zero b=error "NAJJJJJJ"

--Exercise 2, data Ordering = LT |EQ |GT, define occurs''
compare':: Ord a =>a -> a -> Ordering
compare' m n |m==n =EQ
            |m>n  =GT
            |m<n  =LT
            
occurs'':: Int->Tree->Bool
occurs'' m (Leaf n) = compare' m n==EQ
occurs'' m (Node l n r)
           |compare' m n==EQ =True
           |compare' m n==LT =occurs'' m l
           |otherwise        =occurs'' m r

--Exercise 3, define balanced
data Tree5= Leaf5 Int| Node5 Tree5 Tree5

fulltree::Int->Tree
fulltree 0=Leaf 0
fulltree x=Node (fulltree (x-1)) x (fulltree (x-1))

leafnumber::Tree->Int
leafnumber (Leaf n)    =1
leafnumber (Node l n r)=(leafnumber l)+1+(leafnumber r)

balanced:: Tree->Bool
balanced (Leaf n)=True
balanced (Node l _ r)|leafnumber l==leafnumber r    =True
                     |(leafnumber l)+1==leafnumber r=True
                     |leafnumber l==(leafnumber r)+1=True
                     |otherwise=False

--Exercise 4, define balance
fabsplit:: [a]->([a],[a])
fabsplit [] =([],[])
fabsplit (x:(y:xs))|1==length (y:xs)=([x],(y:xs))
                   |2==length (y:xs)=([x],(y:xs))
                   |null (y:xs)     =([x],(y:xs))
                   |otherwise       =fabsplit (x:y:xs)
--balance:: [Int]->Tree
