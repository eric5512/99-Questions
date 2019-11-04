import Data.List (nub)
import System.Random (randomRs, getStdGen)
data NestedList a = Elem a | List [NestedList a]
data Encoded a = Multiple Int a | Single a
    deriving (Show)

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
encode xs = [(length ps, head ps) | ps <- pack xs]

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified xs = [if length ps == 1 then Single (head ps) else Multiple (length ps) (head ps) | ps <- pack xs]

decodeModified :: [Encoded a] -> [a]
decodeModified xs = concat [decodeHelper ps | ps <- xs]
    where 
        decodeHelper (Multiple n e) = replicate n e
        decodeHelper (Single e) = [e]

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [p | (i,p) <- dropEveryAux xs, i `mod` n /= 0]
    where
        dropEveryAux xs = zip [1..] xs

split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split (x:xs) n
    | n > 0 = ((x : (fst (split xs (n - 1))), snd (split xs (n - 1) ) ) )
    | otherwise = (fst (split xs (n - 1)), (x : (snd (split xs (n - 1) ) ) ) ) 

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) 0 0 = [x]
slice (x:xs) 0 n2 = [x] ++ slice xs 0 (n2 - 1)
slice (x:xs) n1 n2 = slice xs (n1 - 1) (n2 - 1)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n
    | n > 0 = rotate (xs++[x]) (n - 1)
    | otherwise = rotate (x:xs) (length (x:xs) + n)

removeAt :: Int -> [a] -> (a,[a])
removeAt n xs = (xs !! (n - 1), [y | (i, y) <- zip [1..] xs, i /= n])

insertAt :: a -> [a] -> Int -> [a]
insertAt _ xs 0 = xs
insertAt y xs 1 = [y] ++ (insertAt y xs 0) 
insertAt y (x:xs) n = [x] ++ (insertAt y xs (n-1))

range :: Int -> Int -> [Int]
range n1 n2
    | n1 < n2 = [n1] ++ (range (n1+1) n2)
    | otherwise = [n1]

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do 
    gen <- getStdGen
    return $ take n [xs !! x | x <- randomRs (0, length xs - 1) gen]

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
    gen <- getStdGen
    return $ take n [x | x <- randomRs (1, m) gen]

rndPermu :: [a] -> IO [a]
rndPermu xs = do
    gen <- getStdGen
    return $ [xs !! x | x <- nub $ take (length xs * 2) (randomRs (0, length xs -1) gen)]

