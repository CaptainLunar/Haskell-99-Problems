-- File:            99Problems.hs
-- Author:          lunarmuffins
-- Description:     Solutions to the Haskell 99 Problems 
-- Problems:        https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

module Problems where

import Data.List
import Data.Char
import System.Random

-- Data Structures
data NE a b = Single b | Multiple a b deriving (Show)
data NestedList a = Elem a | List [NestedList a]

-- Test Lists for problems
testListNum  = [3,1,2,3,4,2,1,2,3,2,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4]
testListChar = "aaaaabbbbbccccdddddeeeeffffggggagadfg"

-- Problems 1 - 10
-- Lists --

-- Problem 1
-- Find the last element of a list.
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x:xs) = Just (last xs)

-- Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Just x
myButLast (x:xs) = Just (last (init xs)) 
 
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
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome ls
    | ls == reverse ls = True
    | otherwise      = False

-- Alternative to problem 6
isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 [] = False
isPalindrome2 lst
    | lst == reverse lst = True
    | otherwise = False

-- Problem 7 
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list 
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten (x) ++ flatten (List xs) 

-- Problem 8
-- Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress = nub

-- Problem 9
-- Packconsecutive duplicates of list elements into sublists. 
-- If  list contains repeated elements they should be placed in separate sublists
pack :: (Ord a) => [a] -> [[a]]
pack = group

-- Problem 10 
-- Run-length encoding of a list. Use the result of problem 9 to implement
-- the so called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Ord a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack 

-- Problem 11
-- Modified run-length encoding
-- Modify problem 10 such that if an element has no duplicates it is simply copied into the result list.
encoder :: (Ord a, Num a) => [(a, b)] -> [NE a b]
encoder [] = []
encoder ((x,y):xs)
    | x == 1    = Single y : encoder xs
    | otherwise = Multiple x y : encoder xs

-- To do
-- Decode the list into results after calling encoder
--rLenEncode :: [NE a b] -> [(Int, b)]

-- Problem 12
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version
ucEncoder :: [NE a b] -> [b]
ucEncoder [] = []
ucEncoder ((Single x):xs)      = x : ucEncoder xs
ucEncoder ((Multiple _ x):xs)  = x : ucEncoder xs

-- Problem 13
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates 
-- (as in problem 9) but only count them. As in P11, simplify the result list by
-- replacing the singleton lists (1 X) by X.

-- Problem 14
-- Duplicate the elements of a list
dupList :: [a] -> [a]
dupList [] = []
dupList (x:xs) = x : x : dupList xs

-- Problem 15 
-- Replicate the elements of a list a given number of times

-- Problem 16
-- Drop every N'th element from a list

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicatees

-- Problems 18 -> NEED TO FIX
-- Extract a slice from a list given two indices
--slice' :: [a] -> Int -> Int -> [a]
--slice' [] _ _ = []
--slice' _ 0 k = 
--slice' (y:ys) i j = [x | x <- slice' ys (i-1) j]

-- Problems 21 - 28
-- Lists again

-- Problem 21
-- Insert an element at a given position into a list.

-- Problem 22
-- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range x y = [x..y]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.

------------------------------------------------------------------------
-- Problems 31 - 41 
-- Arithmetic --

-- Problem 31
-- Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime x = factors x == [1, x]
    where factors b = [y | y <- [1..x], b `mod` y == 0]

-- Problem 32 
-- Determine the greatest common divisor of two positive integer numbers
gcd' :: Int -> Int -> Int
gcd' x 0 = x 
gcd' x y = gcd' y (x `mod` y)

-- Problem 33
-- Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1
coprime :: Int -> Int -> Bool
coprime x y = gcd' x y == 1

-- Problems 34
-- Calculate Euler's totient function phi(m)
phi :: Int -> Int
phi 1 = 1
phi n = length [x | x <- [1..n-1], x `coprime` n]

--Problem 35 -> NEED TO FIX
--Determine the prime factors of a given positive integer.
--Construct a flat list containing the prime factors in ascending order.
-- Ex ) primeFactors 315 => [3,3,5,7]
primeFactors :: Int -> [Int]
primeFactors n = [x | x <- [1..n], isFactor $ gcd n x]

isFactor :: Int -> Bool
isFactor x
    | x `mod` 2 == 0 = True
    | otherwise = False

-- Problem 36
-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicty.
