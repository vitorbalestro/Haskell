

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a -- inserts an element into an empty tree
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a -- inserts an element into a tree
treeInsert x EmptyTree = singleton x
treeInsert x (Node n left right)
    | x == n = Node x left right
    | x < n = Node n (treeInsert x left) right
    | x > n = Node n left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool -- tells whether a given value is in a given tree
treeElem _ EmptyTree = False
treeElem x (Node n left right)
    | x == n = True
    | x < n = treeElem x left
    | x > n = treeElem x right

treeFromList :: (Ord a) => [a] -> Tree a -- builds a tree from a list
treeFromList = foldr treeInsert EmptyTree

listFromTree :: (Ord a) => Tree a -> [a] -- builds a sorted list from a tree
listFromTree EmptyTree = []
listFromTree (Node n left right) = listFromTree left ++ (n : listFromTree right)

treeMax :: (Ord a) => Tree a -> Maybe a
treeMax EmptyTree = Nothing
treeMax (Node x left right)
    | right == EmptyTree = Just x
    | otherwise = treeMax right

treeMin :: (Ord a) => Tree a -> Maybe a
treeMin EmptyTree = Nothing
treeMin (Node x left right)
    | left == EmptyTree = Just x
    | otherwise = treeMin left

treeRemove :: (Ord a) => a -> Tree a -> Tree a -- removes a node from a tree (no balancing concerns)
treeRemove _ EmptyTree = EmptyTree
treeRemove x (Node n left right) 
    | x < n = Node n (treeRemove x left) right
    | x > n = Node n left (treeRemove x right)
    | x == n = treeFromList (listFromTree left ++ listFromTree right)


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ EmptyTree = EmptyTree
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

