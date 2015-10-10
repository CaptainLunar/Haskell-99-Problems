module Problems where
-- Haskell 99 Problems Solutions & Functions 

import Data.List
import Data.Char
import Data.Bits
import Data.Binary
import Numeric

--import Data.Digits

-- Problem 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Can't have an empty list!"
myLast [x] = x
myLast (x:xs) = last xs

-- Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Can't have an empty list!"
myButLast [x] = x
myButLast (x:xs) = last (init xs) 

-- Problem 3
-- Find the kth element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) y = elementAt xs (y-1)

-- Problem 4
-- Find the number of elements of a list.
numElements :: [a] -> Int
numElements [] = 0
numElements xs = length xs

-- Problem 5
-- Reverse a list.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Alternative to problem 5
reverseList2 :: [a] -> [a]
reverseList2 [] = []
reverseList2 lst = reverse lst

-- Problem 6
-- Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> String
isPalindrome [] = "Empty lists aren't a palindrome"
isPalindrome lst
	| lst == reverse lst = "Is a palindrome"
	| otherwise = "Not a palindrome"

-- Alternative to problem 7
isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 [] = False
isPalindrome2 lst
	| lst == reverseList lst = True
	| otherwise = False

-- Problem 7 
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list 
-- by replacing each list with its elements (recursively).
-- join :: Monad m => m (m a) -> m a
-- data BigBoss = JUST | A | BIG | GUY deriving (Show)
--toDec :: String -> Int
--toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
--myBinAdd :: (Int, Int) -> IO ()
--myBinAdd (x, y) = 
--  if y == 0
--    then putStrLn $ showIntAtBase 2 intToDigit x ""
--    else myBinAdd(xor x y, shiftL (x.&.y) 1)

-- lel takes a 2-tuple string of binary numbers and adds them together
-- 252 characters
lel :: (String, String) -> IO ()
lel (x, y) = b(d(x), d(y))
    where d = foldl' (\c x -> c * 2 + digitToInt x) 0 
          b (x, y) = if y == 0
            then putStrLn $ showIntAtBase 2 intToDigit x ""
            else b(xor x y, shiftL (x.&.y) 1) 

-- 284 characters on Ben's code
binAdd :: [Int] -> [Int] -> [Int]
binAdd xs ys = reverse . binAdd' . reverse $ zipWith (+) (pad xs) (pad ys)
  where
    binAdd' (n:ns) = map snd $ scanl f (divMod n 2) ns
    f (c, r) b = divMod (b + c) 2
    pad zs = replicate (abs $ length zs - max (length xs) (length ys)) 0 ++ zs

--myBinAdd2 :: ()
--toBinary :: Int -> Int
--toBinary n = 
--  if n > 0
--    then n
--    otherwise (n mod 2)



--binAdd :: [Int] -> [Int] -> [Int]
--binAdd xs ys = binAdd' $ zipWith (+) (pad xs) (pad ys)
--where
--   binAdd' (n:ns) = map snd $ scanr g (divMod n 2) ns
--   g b (c, r) = divMod (b + c) 2
--   pad zs = replicate (abs $ length zs - max (length xs) (length ys)) 0 ++ zs