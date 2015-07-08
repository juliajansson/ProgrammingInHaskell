--1) halve splits an even-lengthed list into two halves
--Help with the else case? I want an error message.
halve :: [a] -> ([a],[a])
halve as = if even (length as)
           then splitAt (div (length as) 2) as
           else error "Not an even-lengthed list!"

-- 2a) safetail defined by a conditional expression
-- safetail maps the empty list to itself, i can't make it work
safetail :: [a] -> [a]
safetail (a:as) = if null (a:as)   -- PJ: This is never true! Try as instead of a:as
                  then []
                  else drop 1 (a:as)

-- 2b) safetail defined by guarded equations
-- i don't know what's wrong
safetail2 :: [a] -> [a]
safetail2 (a:as)|null (a:as)==False = drop 1 (a:as)
                |otherwise          = []

-- 2c) safetail defined by pattern matching
safetail3:: [a] -> [a]
safetail3 [] = []
safetail3 (_:as) = as

{-
-- 3) define || in 4 different ways using pattern matching
-- 3a)
(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False

-- 3b)
(||) :: Bool -> Bool -> Bool
False||False=False
_    ||_    =True

-- 3c) What's up with the b? (PJ: Haskell does not allow using b twice on the left hand side.)
(||) :: Bool -> Bool -> Bool
b    ||b    =b
True ||_    =True
_    ||True =True
-}

-- 3d)
(||) :: Bool -> Bool -> Bool
b||c | b==c       = b
     | otherwise  = True
{-
-- 4 redefine the && operator with conditional expressions
(&&) :: Bool -> Bool -> Bool
(&&) a b  = if a==b==True then True
            else False

-- 5 redefine the && operator from another version
(&&):: Bool -> Bool -> Bool
(&&) a b  = if a==True then b
            else False

-- 6 define mult x y z=x*y*z in lambda expression
mult = (\x -> (\y -> (\z -> x*y*z)))

-}
