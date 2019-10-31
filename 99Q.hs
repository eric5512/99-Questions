data NestedList a = Elem a | List [NestedList a]

myLast :: [a] -> a
myLast [] = error "Empy list"
myLast [x] = x
myLast (_:xs) = myLast xs


myButLast :: [a] -> a
myButLast [_] = error "Only one element"
myButLast [] = error "Empty list"
myButLast (x:xs)
    | length xs == 1 = x
    | otherwise = myButLast xs


elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list or index out of range"
elementAt (x:xs) 0 = x
elementAt (x:xs) n = elementAt xs (n-1)

myLenght :: [a] -> Int
myLenght [] = 0
myLenght (x:xs) = 1 + myLenght xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: Eq a => [a] -> [a]
compress (x:[]) = [x]
compress [] = []
compress (x:xs)
    | x == head xs = compress xs
    | otherwise = [x] ++ compress xs

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first): pack rest
    where
        getRepl [] = ([], [])
        getRepl (y:ys)
            | y == x = let(f, r) = getRepl ys in (y:f, r)
            | otherwise = ([],(y:ys))
        (first, rest) = getRepl xs

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:[]) = [(1, x)]
encode (x:xs) = [(length p, head p)] ++ encode xs
    where
        (p:ps) = pack xs