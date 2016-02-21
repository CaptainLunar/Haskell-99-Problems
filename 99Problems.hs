-- Haskell 99 Problems Solutions & Functions 
-- lunar muffins: All rights reserved 2015
module Problems where

import Data.List
import Data.Char

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
reverseList2 l = reverse l

-- Problem 6
-- Find out whether a list is a palindrome. 
isPalindrome :: (Eq a) => [a] -> String
isPalindrome [] = "Empty lists aren't a palindrome"
isPalindrome l
    | l == reverse l = "Is a palindrome"
    | otherwise = "Not a palindrome"

-- Alternative to problem 6
isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 [] = False
isPalindrome2 lst
    | lst == reverse lst = True
    | otherwise = False

-- Problem 7 
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list 
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten (x) ++ flatten (List xs) 

-- Problem 8
-- Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress = nub

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists
-- Example (a a a a a a b c c c c d e e e ) 
-- => ((a a a a a a) (b) (c c c c) (d) (e e e ))
pack :: (Ord a) => [a] -> [[a]]
pack = group

-- Problem 10 
-- Run-length encoding of a list. Use the result of problem 9 to implement
-- the so called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number
-- of duplucates of the element E.
-- Example (encode `(a a a a b c c a a d e e e))
-- => ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
encode :: (Ord a) => [[a]] -> [(Int, a)]
encode = map (\x -> (length x, head x))

 
