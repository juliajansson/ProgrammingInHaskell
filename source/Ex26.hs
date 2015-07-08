najs :: Int
najs = div a (length xs)
         where
           a  = 10
           xs = [1,2,3,4,5]
-- Tre funktioner som alla plockar det sista elementet i en lista
-- Nr1
mylast :: [Int] -> Int
mylast [] = 0
mylast (x:xs) = head (reverse (x:xs))

-- Nr2
sist :: [Int] -> Int
sist [] = 0
sist (x:xs) = (x:xs)!!(length xs)

-- En listig funktion
avlista :: [Int] -> Int
avlista [] = 0
avlista (x:[]) = x

--Nr3
letzte :: [Int] -> Int
letzte [] = 0
letzte (x:xs) = avlista (drop (length xs) (x:xs))

-- Tre funktioner som tar bort det sista elementet i en lista
-- Nr1
myinit :: [Int] -> [Int]
myinit [] = []
myinit (x:xs) = take (length xs) (x:xs)

-- Nr2
bortmedsisten :: [Int] -> [Int]
bortmedsisten [] = []
bortmedsisten (x:xs) = reverse(tail(reverse (x:xs)))

-- Nr3
weckletzte :: [Int] -> [Int]
weckletzte [] = []
weckletzte (x:xs) = take (length xs) (x:xs)
