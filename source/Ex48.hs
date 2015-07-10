
--1) halve splits an even-lengthed list into two halves
halve :: [a] -> ([a],[a])
halve as = if even (length as)
           then splitAt (div (length as) 2) as
           else error "Not an even-lengthed list!"

-- 2a) safetail defined by a conditional expression
safetail :: [a] -> [a]
safetail as = if null as
              then []
              else drop 1 as

-- 2b) safetail defined by guarded equations
safetail2 :: [a] -> [a]
safetail2 as | not (null as)  = drop 1 as
             | otherwise      = []

-- 2c) safetail defined by pattern matching
safetail3:: [a] -> [a]
safetail3 [] = []
safetail3 (_:as) = as


-- 3) define || in 4 different ways using pattern matching
-- 3a)
(||!) :: Bool -> Bool -> Bool
True  ||! True  = True
True  ||! False = True
False ||! True  = True
False ||! False = False

-- 3b)
(||?) :: Bool -> Bool -> Bool
False ||? False = False
_     ||? _     = True

-- 3c)
(||*) :: Bool -> Bool -> Bool
b    ||* c | b == c = b
True ||* _    = True
_    ||* True = True


-- 3d)
(||#) :: Bool -> Bool -> Bool
b ||# c | b==c       = b
        | otherwise  = True

-- 4 redefine the && operator with conditional expressions
(&&!) :: Bool -> Bool -> Bool
a &&! b  = if a==b
           then a
           else False

-- 5 redefine the && operator from another version
(&&?) :: Bool -> Bool -> Bool
a &&? b  = if a then b
           else a

-- 6 define mult x y z=x*y*z in lambda expression
mult :: Num a => a -> ( a -> ( a -> a    ))
mult =          \x -> (\y -> (\z -> x*y*z))

mult' :: Num a => a ->  a ->  a -> a
mult' =          \x -> \y -> \z -> x*y*z

mult'' :: Num a => a -> a -> a -> a
mult'' =          \x    y    z -> x*y*z

--Lambda expression for odds
odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]
