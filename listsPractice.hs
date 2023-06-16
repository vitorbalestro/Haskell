import Data.Maybe

listTest xs = [ x | x <- [0..20] , even x ]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

binary5digits = [[x1,x2,x3,x4,x5] | x1 <- [0,1], x2 <- [0,1], x3 <- [0,1], x4 <-[0,1], x5 <- [0,1]]

length' xs = sum [1 | x <- xs]

quick :: (Ord a) => [a] -> [a]
quick [] = []
quick [x] = [x]
quick (x:xs) = quick[y | y <- xs, y <= x] ++ [x] ++ quick[y | y <- xs, y > x]

calcBmi :: (RealFloat a) => [(a,a)] -> [a]
calcBmi xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) 
    | x > maximum' xs = x
    | otherwise = maximum' xs 

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n < 0 = error "invalid argument"
    | n == 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take'(n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' c (x:xs)
    | c x = x:filter' c xs
    | otherwise = filter' c xs

id' :: a -> a
id' x = x

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _  [] = []
takeWhile' c  (x:xs)
    | c x = x : takeWhile' c  xs
    | otherwise = []


collatzChain :: (Integral a) => a -> [a]
collatzChain x 
    | x == 1 = [1]
    | even x = x : collatzChain (x `div` 2)
    | odd x = x : collatzChain (3*x + 1)
    | otherwise = error "invalid argument."

intersperce' :: a -> [a] -> [a]
intersperce' _ [] = []
intersperce' _ [x] = [x]
intersperce' c (x:xs) = x:c:intersperce' c xs

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' _ [[x]] = [x]
intercalate' y (x:xs) = x ++ y ++ intercalate' y xs

toListOfLists :: [a] -> [[a]]
toListOfLists [] = []
toListOfLists x = [[y] | y <- x]

zipTogether' :: [[a]] -> [[a]] -> [[a]]
zipTogether' [] x = x
zipTogether' x [] = x
zipTogether' (x:xs) (y:ys) = (x ++ y) : zipTogether' xs ys


transpose' :: [[a]] -> [[a]] -- transposes a matrix
transpose' [] = []
transpose' (x:xs) = zipTogether' (toListOfLists x) (transpose' xs) 

transpose'' :: [[a]] -> [[a]] -- transposes a matrix (better logic than above)
transpose'' = foldr (zipTogether' . toListOfLists) []

concat' :: [[a]] -> [a] -- concatenates a list of lists into a single list
concat' [] = []
concat' xs = foldl1 (++) xs 

and' :: [Bool] -> Bool
and' = foldl (&&) True 

iterate' :: (a -> a) -> a -> [a] -- returns the infinite list of ordered iterations of a function over a domain value
iterate' f x = f x : iterate' f (f x) -- this leads to an infinite list!

group' :: (Eq a) => [a] -> [[a]] -- groups up adjacent equal elements of a given list
group' [] = []
group' xs = let (fw,rest) = span (== head xs) xs in fw : group' rest

howManyOfEach :: (Integral i, Integral a) => [a] -> [(a,i)] -- returns the list of tuples (element, frequency) of a given list
howManyOfEach = map (\x -> (head x, length' x)) . group' . quick

inits' :: [a] -> [[a]] -- returns the list of inits of a given list
inits' = foldl (\acc x -> acc ++ [last acc ++ [x]]) [[]] 

tails' :: [a] -> [[a]] -- returns the list of tails of a given list
tails' = foldr (\x acc -> (x : head acc) : acc) [[]]

findSublist :: (Eq a) => [a] -> [a] -> Bool -- same as below, but with worst logic
findSublist [] _ = True
findSublist _ [] = False
findSublist xs ys = xs `elem` inits' ys || xs `elem` tails' ys || xs `elem` inits' (tail (init ys)) || xs `elem` tails' (tail (init ys))

search :: (Eq a) => [a] -> [a] -> Bool -- tells whether a given list is a sublist of another list
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> 
        take nlen x == needle || acc) False (tails' haystack)

partition' :: (a -> Bool) -> [a] -> ([a],[a]) -- partitionates a given list according to a given predicate
partition' _ [] = ([],[])
partition' c xs = (true,false)
    where 
        true = foldr (\x acc -> if c x then x:acc else acc) [] xs
        false = foldr (\x acc -> if c x then acc else x:acc) [] xs

find' :: (a -> Bool) -> [a] -> Maybe a -- returns the first element of a given list which satisfies a given predicate
find' _ [] = Nothing
find' c xs = foldl (\acc x -> if c x then Just x else acc) Nothing xs

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int -- returns the index of the first ocurrence of a given element in a given list
elemIndex' _ [] = Nothing
elemIndex' x xs = if lengthUntilFind < totalLength then Just lengthUntilFind else Nothing
    where
        lengthUntilFind  = length . takeWhile' (/=x) $ xs
        totalLength = length xs

removeElems :: (Integral i) => i -> [a] -> [a] -- remove the first n elements from a list
removeElems _ [] = []
removeElems 0 x = x
removeElems 1 x = tail x
removeElems n xs = removeElems (n-1) (tail xs)

elemIndices' :: (Eq a) => a -> [a] -> [Int] -- returns all the positions where a given element occurs in a list
elemIndices' _ [] = []
elemIndices' x xs  
    | x `notElem` xs = []
    | otherwise = 
        let foundPosition = fromJust(elemIndex' x xs) 
        in foundPosition : map (+(foundPosition+1)) (elemIndices' x (removeElems (foundPosition + 1) xs))

split' :: Char -> String -> [String] -- takes a string and split it by a given character
split' c x
    | notElem c x = [x]
    | otherwise = takeWhile (/= c) x : split' c (removeElems (length (takeWhile (/=c) x) + 1) x)

lines' :: String -> [String] -- put the lines of a given string in a list
lines' = split' '\n'

unlines' :: [String] -> String -- takes a list of strings and concatenate them using \n
unlines' [] = ""
unlines' xs = foldl1 (\acc x -> acc ++ "\n" ++ x) xs        


multipleSplit' :: [Char] -> String -> [String] -- split a given string by multiple characters
multipleSplit' c x
    | null (filter' (`elem'` c) x) = [] 
    | otherwise = takeWhile (`notElem` c) x : multipleSplit' c (removeElems (length (takeWhile (`notElem` c) x) + 1) x)

nub' :: (Eq a) => [a] -> [a] -- takes a list and returns the list of its distinct values
nub' [] = []
nub' x = foldr (\x acc -> if x `notElem` acc then x:acc else acc) [] x

deleteAll' :: (Eq a) => a -> [a] -> [a] -- deletes all ocurrences of a given element from a given list
deleteAll' _ [] = []
deleteAll' c x = foldr (\x acc -> if x == c then acc else x:acc) [] x

delete' :: (Eq a) => a -> [a] -> [a] -- deletes the first ocurrence of a given element from a given list
delete' _ [] = []
delete' c x = firstBlock ++ removeElems (removedLength + 1) x
    where
        firstBlock = takeWhile' (/= c) x
        removedLength = length firstBlock

removeFromList' :: (Eq a) => [a] -> [a] -> [a] -- removes from a given list the first ocurrence of each element of another given list
removeFromList' = foldl (flip delete')  

insert' :: (Ord a) => a -> [a] -> [a] -- inserts an element x in a list l just before the first element of l equal to or greater than x
insert' x [] = [x]
insert' c x = takeWhile' (< c) x ++ [c] ++ removeElems (length . takeWhile' (<c) $ x) x

splitAt' :: (Integral i) => i -> [a] -> ([a],[a]) -- splits a given list in a given position
splitAt' _ [] = ([],[])
splitAt' n x = (take' n x, removeElems n x)

-- below, we deal with maps

findKey' :: (Eq k) => k -> [(k,v)] -> v
findKey' key  = snd . head . filter' (\(k,v) -> key == k ) 

mapKeys' :: (Eq k) => [(k,v)] -> [k]
mapKeys' = foldr (\x acc -> fst x : acc) [] 

