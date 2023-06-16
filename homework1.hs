-- CREDIT CARD NUMBER VALIDATION

arrayLength :: [a] -> Integer
arrayLength [] = 0
arrayLength (x:xs) = 1 + arrayLength(xs)

-- exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = digit:toDigitsRev rest
            where 
                digit = x `mod` 10
                rest = (x - x `mod` 10) `div` 10

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | otherwise = toDigits rest ++ [digit]
        where
            digit = x `mod` 10
            rest = (x - x `mod` 10) `div` 10

-- remark: we could also have used the following revert function:

revertList :: [a] -> [a]
revertList = foldr(\x acc -> acc ++ [x]) []

-- exercise 2 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = foldr(\x acc -> if arrayLength(acc) `mod` 2 /= 0 then 2*x:acc else x:acc) [] 

-- exercise 3

sumDigits :: [Integer] -> Integer -- exercise 3
sumDigits = foldr(\x acc -> acc + (x `mod` 10) + ((x - x `mod` 10) `div` 10)) 0

-- exercise 4


validity :: Integer -> Bool
validity x = sumDigits(doubleEveryOther(toDigits x)) `mod` 10 == 0

-- HANOI TOWER

-- exercise 5

type Peg = String
type Move = (Peg,Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 1 = [(a,b)]
    | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- exercise 6
-- We give several algorithms. They are increasingly more optimal. The last one was not my idea.

hanoiWithFourPegs :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiWithFourPegs n a b c d 
    | n == 0 = []
    | n == 1 = [(a,d)]
    | otherwise = hanoiWithFourPegs (n-2) a b d c ++ [(a,b),(a,d),(b,d)] ++ hanoiWithFourPegs (n-2) c a b d

hanoiWithFourPegs' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiWithFourPegs' n a b c d
    | n == 0 = []
    | n == 1 = [(a,d)]
    | otherwise = hanoiWithFourPegs' (half) a b d c ++ hanoi (n - half) a b d ++ hanoiWithFourPegs' (half) c a b d
        where 
            half 
                | even n = n `div` 2
                | otherwise = n `div` 2 + 1

hanoiWithFourPegs'' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiWithFourPegs'' n a b c d
    | n <= 0 = []
    | n == 1 = [(a,d)]
    | n == 2 = [(a,b),(a,d),(b,d)]
    | otherwise = hanoiWithFourPegs'' (third) a b d c ++ hanoi (third) a d b
    ++ hanoiWithFourPegs'' (third) c a d b ++ hanoi (n - 2 * third) a c d ++ hanoiWithFourPegs'' (2 * third) b a c d
    where
        third = n `div` 3

-- below, we implement a better solution based on the paper available at https://dl.acm.org/doi/pdf/10.1145/126459.126460
-- this has complexity O(4^sqrt(n))

sumOfFirstNaturals :: (Integral a) => a -> a -- gives the sum of the first k natural numbers
sumOfFirstNaturals k
    | k == 1 = 1
    | otherwise = k + sumOfFirstNaturals(k-1)

sumint :: (Integral a) => a -> a -- returns the greatest k such that the sum from 1 to k is lesser than or equal to n
sumint n 
    | n == 1 = 1
    | otherwise = if sumOfFirstNaturals k + k + 1 > n then k else k+1
        where  
            k = sumint(n-1)

hanoiWithFourPegs''' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiWithFourPegs''' n a b c d
    | n == 1 = [(a,d)]
    | otherwise = hanoiWithFourPegs''' (n-k) a c d b ++ hanoi k a c d ++ hanoiWithFourPegs''' (n-k) b a c d
        where
            k = sumint n 