{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:           Burak Inan
VU-net id:      bin230
Student number: 2672032
Discussed with: 
Remarks:        
Sources:   
 https://www.techrepublic.com/article/infinite-list-tricks-in-haskell/
 https://www.schoolofhaskell.com/user/henryk/infinite-subsets-of-natural-numbers
 https://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell
 The exercise notes, section 4.4
 http://learnyouahaskell.com/zippers

-}

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = 1 : map (\x -> x + 1) naturals

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = zeroesandones' 1
zeroesandones' :: Integer -> [Integer]
zeroesandones' x = if (x == 1) then (x - 1) : zeroesandones' 0 else (x + 1) : zeroesandones' 1

-- Exercise 3
threefolds :: [Integer]
threefolds = 0 : map (\x -> x * 3) naturals

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif f = filter (not.f)

nothreefolds :: [Integer]
nothreefolds = removeif (\x -> x == 0 || mod x 3 == 0) naturals

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds n = map (\x -> x * n) naturals

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds n = removeif (\x -> mod x n == 0) naturals

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n list = removeif (\x -> mod x n == 0) list

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = eratosthenes' (removeif (< 2) naturals)
eratosthenes' (h:t) = h : eratosthenes' [x | x <- t, mod x h /= 0]

-- Exercise 9
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- -----------------------
-- Exercise Church Numerals
-- -----------------------
-- we need polymorphic types for the Church Numerals 
type ChurchNumeral = forall a . (a -> a) -> a -> a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral 
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral -> a
backtointeger cn = cn (+1) 0

{- 
    Returns 12345:
    backtointeger (churchnumeral 12345)

    Returns 30:
    backtointeger (churchnumeral (5 * 2 * 3))
 -}

-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = backtointeger x == backtointeger y

-- Exercise 3 given as example
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s (x s z)
 
-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = x s (s z)

-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger (f (churchnumeral n)) 

-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z  = x s (y s z)

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s  = x (y s)

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = y x

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = backtointeger (f (churchnumeral n) (churchnumeral m)) 


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- helper
single :: a -> BinaryTree a
single x = Node (Leaf) x (Leaf)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes Leaf = 0
numberofnodes (Node l el r) = 1 + (numberofnodes l) + (numberofnodes r)

-- Exercise 2
height :: BinaryTree a -> Integer
height Leaf = 0
height (Node l el r) = 1 + (abs ((height l) - (height r)))

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes Leaf = 0
sumnodes (Node l el r) = el + (sumnodes l) + (sumnodes r)

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror Leaf = Leaf
mirror (Node l el r) = Node (mirror r) el (mirror l)

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten Leaf = []
flatten (Node l el r) = (flatten l) ++ [el] ++ (flatten r)

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f Leaf = Leaf
treemap f (Node l el r) = Node (treemap f l) (f el) (treemap f r)

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan x Leaf = True
smallerthan x (Node l el r) = x > el && (smallerthan x l) && (smallerthan x r)

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan x Leaf = True
largerthan x (Node l el r) = x < el && (largerthan x l) && (largerthan x r)

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree Leaf = True
isbinarysearchtree (Node l el r) = 
    smallerthan el l && largerthan el r && isbinarysearchtree l && isbinarysearchtree r

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement x Leaf = False
iselement x (Node l el r) = 
    if x < el then
        iselement x l
    else if x > el then
        iselement x r
    else x == el

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l el r) = 
    if (iselement x (Node l el r)) then
        (Node l el r)
    else if (x < el) then
        (Node (insert x l) el r)
    else (Node l el (insert x r))

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree [] = Leaf
createbinarysearchtree (h:t) = createbinarysearchtree' t (Node Leaf h Leaf)
createbinarysearchtree' (h:[]) (Node l el r) = insert h (Node l el r)
createbinarysearchtree' (h:t) (Node l el r) = createbinarysearchtree' t (insert h (Node l el r))

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove = undefined


----------------------------
-- Exercise Tower of Hanoi
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi = undefined
