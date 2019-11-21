module Practicum1 where

{-

Name:           Burak Inan
VU-net id:      bin230
Student number: 2672032

Discussed with: 
 - Ronan Hochart
 - Basel Sammour

Remarks:        
 - None

Sources:        

 - http://zvon.org/other/haskell/Outputprelude/ (multiple pages from this website)
 - http://learnyouahaskell.com/recursion
 - https://wiki.haskell.org/The_Fibonacci_sequence
 - https://stackoverflow.com/questions/41124599/auxiliary-function-in-haskell
 - https://stackoverflow.com/questions/26847192/reverse-a-list-in-haskell
 - https://stackoverflow.com/questions/48696720/checking-to-see-if-an-element-exists-in-a-list-in-haskell
 - https://stackoverflow.com/questions/8712208/how-do-i-use-the-filter-function-in-haskell
 - https://www.youtube.com/watch?v=wObxd4Kx8sE
 - https://mmhaskell.com/blog/2019/5/13/quicksort-with-haskell
 - https://wiki.haskell.org/List_comprehension
 - https://wiki.haskell.org/Fold 

-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi x y = if x >= y then x else y

-- maxi 2 3 == 3
-- maxi 3 2 == 2

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending a b c d = a < b && b < c && c < d

-- Test 1 - returns True:
-- fourAscending 1 2 3 4
-- Test 2 - returns True:
-- fourAscending (-20) (-5) 0 1
-- Test 3 - returns False:
-- fourAscending 4 3 2 1

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a == b && b == c && c == d

-- Test 1 - returns True:
-- fourEqual 4 4 4 4
-- Test 2 - returns False:
-- fourEqual 1 2 3 4

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent a b c d = a /= b && a /= c && a /= d  && b /= c && b /= d && c /= d

-- Exercise 5
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent a b c = ((a /= b) && (b /= c))

{-
   The function does not check if ALL arguments are different.
   It only checks them consecutively (a diff b, b diff c, etc).
   threeDifferent 3 4 3 will return True, because it does not 
   compare the first argument with the last.
-}

-- Exercise 6
factorial :: Integer -> Integer
factorial n = if n < 2 then 1 else n * factorial (n - 1)

-- Test 1 - returns 1:
-- factorial 1
-- Test 2 - return 120:
-- factorial 5

-- Exercise 7
fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

-- Test 1 - returns 55:
-- fib 10
-- Test 2 - returns 1:
-- fib 2
-- Test 3 - returns 1:
-- fib 1

-- Exercise 8
-- it is possible to define auxiliary functions
aux :: Integer -> Integer -> Integer
aux a b = if a <= b then a + aux (a + 1) b else 0
strangeSummation :: Integer -> Integer
strangeSummation n = aux n (n + 7)

-- Test 1 - returns 36:
-- strangeSummation 1
-- Test 2 - returns 52:
-- strangeSummation 3

-- Exercise 9
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

lengthListAlternative :: [Integer] -> Integer
lengthListAlternative l =
  case l of
    [] -> 0
    (h:t) -> 1 + (lengthListAlternative t)

sumList :: [Integer] -> Integer
sumList l = case l of 
  [] -> 0
  (x:xs) -> x + sumList xs

-- Test 1 - returns 6:
-- sumList [1,2,3]
-- Test 2 - returns 0:
-- sumList []

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList l = case l of
  [] -> []
  (x:xs) -> (x * 2) : (doubleList xs)

-- Test 1 - returns [2,4,6]
-- doubleList [1,2,3]
-- Test 2 - returns []
-- doubleList []
-- Test 3 - returns [10,20,30]
-- doubleList [5,10,15]

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend [] ys = ys
myappend (x:xs) ys = x : (myappend xs ys)

-- Test 1 - returns [1,1,2,2]
-- myappend [1,1] [2,2]
-- Test 2 - returns []
-- myappend [] []

-- Exercise 12
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myappend (myreverse xs) [x]

-- Test 1 - returns [4,3,2,1]
-- myreverse [1,2,3,4]
-- Test 2 - returns [0,0,1,1]
-- myreverse [1,1,0,0]

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember x [] = False
mymember x (y:ys) = if (x == y) then True else mymember x ys

-- Test 1 - returns True
-- mymember 1 [1,2,3,4]
-- Test 2 - returns False
-- mymember 1 [2,3,4]

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum [] = 0
mysquaresum (x:xs) = (x ^ 2) + (mysquaresum xs)

-- Test 1 - returns 30
-- mysquaresum [1,2,3,4]
-- Test 2 - returns 1
-- mysquaresum [0,1]

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range a b = if a > b then [] else a : (range (a + 1) b)

-- Test 1 - returns [1,2,3,4,5]
-- range 1 5
-- Test 2 - returns []
-- range 5 0

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ (myconcat xs)

-- Test 1 - returns [1,2,3,4]
-- myconcat [[1,2], [3,4]]
-- Test 2 - returns [9,8,7,6,5]
-- myconcat [[9,8,7], [6,5]]

-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = 
  if x < y then
    x:y:ys
    else
      y:(insert x ys)

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = insert x (insertionsort xs)

-- Test 1 - returns [1,3,5,9]
-- insertionsort [5,9,1,3]
-- Test 2 - returns [2,22,90,90,100]
-- insertionsort [100,2,22,90,90]

-- Exercise 18
quicksort :: Ord a => [a] -> [a]
myfilter :: Ord a => [a] -> [a]
myfilter' :: Ord a => [a] -> [a]
myfilter (h:t) = filter (\x -> x <= h) t
myfilter' (h:t) = filter (\x -> x > h) t
quicksort [] = []
quicksort (h:t) = quicksort (myfilter (h:t)) ++ h:quicksort (myfilter' (h:t))

-- Test 1 - returns [1,3,5,9]
-- quicksort [5,9,1,3]
-- Test 2 - returns [2,22,90,90,100]
-- quicksort [100,2,22,90,90]

-- Exercise 19
evensB :: [Integer] -> [Integer]
evensB xs  = [x | x <- xs, even x]

-- Test 1 - returns [2,4]
-- evensB [1,2,3,4,5]
-- Test 2 - returns []
-- evensb [1,3,5,7,9]

-- Exercise 20
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

-- Test 1 - returns [3,4,5,6]
-- mymap (\x -> x + 2) [1,2,3,4]
-- Test 2 - returns [1.0, 1.0, 1.0, 1.0]
-- mymap (\x -> x / x) [1,2,3,4]

-- Exercise 21
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Test 1 - returns 9
-- twice (\x -> x + 2) 5
-- Test 2 - returns 1
-- twice (\x -> mod x 2) 5

-- Exercise 22
compose :: (b -> c) -> (a -> b) -> a -> c
compose f f' x = f (f' x)

-- Test 1 - returns 12
-- compose (\x -> x + 2) (\x -> x * 2) 5
-- Test 2 - returns 10000
-- compose (\x -> x * x) (\x -> x + x) 50

-- Exercise 23
mylast :: [a] -> a
mylast x = head (myreverse x)

-- Test 1 - returns 4
-- mylast [1,2,3,4]
-- Test 2 - returns 500
-- mylast [0,9,10,500]

-- Exercise 24
mylastb :: [a] -> a
drop' :: [a] -> a
drop' x = if length x == 1 then head x else drop' (drop 1 x)
mylastb x = drop' x

-- Test 1 - returns 5
-- mylastb [1,2,3,4,5]
-- Test 2 - returns 500
-- mylastb [500]

-- Exercise 25
myinit, myinitb :: [a] -> [a]
myinit x = myreverse (tail (myreverse x))
myinitb x = take (length x - 1) x

-- Test 1 - returns [1,2,3,4]
-- myinit [1,2,3,4,5]
-- Test 2 - returns [1,1,1,1]
-- myinit [1,1,1,1,1]
-- Test 3 - returns [5,4,3,3,2,1]
-- myinitb [5,4,3,2,1,0]
-- Test 4 - returns []
-- myinitb []

-- Exercise 26
mysecondconcat :: [[a]] -> [a]
mysecondconcat x = foldr (++) [] x

-- Test 1 - returns [1,2,3,4]
-- mysecondconcat [[1,2], [3,4]]
-- Test 2 - returns [9,8,7,6,5]
-- mysecondconcat [[9,8,7], [6,5]]

mysecondreverse :: [a] -> [a]
mysecondreverse xs = foldr (\x y -> y ++ [x]) [] xs

-- Test 1 - returns [4,3,2,1]
-- mysecondreverse [1,2,3,4]
-- Test 2 - returns [0,0,1,1]
-- mysecondreverse [1,1,0,0]

-- Exercise 27
mythirdconcat :: [[a]] -> [a]
mythirdconcat x = foldl (++) [] x

-- Test 1 - returns [1,2,3,4]
-- mythirdconcat [[1,2], [3,4]]
-- Test 2 - returns [9,8,7,6,5]
-- mythirdconcat [[9,8,7], [6,5]]

mythirdreverse :: [a] -> [a]
mythirdreverse xs = foldl (\x y -> [y] ++ x) [] xs

-- Test 1 - returns [4,3,2,1]
-- mythirdreverse [1,2,3,4]
-- Test 2 - returns [0,0,1,1]
-- mythirdreverse [1,1,0,0]

-- Exercise 28
prefix :: [a] -> [[a]]
prefix [] = [[]]
prefix (x:xs) = [] : (mymap (x:) (prefix xs))

-- Test 1 - returns [[], [1], [1,2], [1,2,3], [1,2,3,4]]
-- prefix [1,2,3,4]
-- Test 2 - returns [[], [0]]
-- prefix [0]
-- Test 3 - returns [[]]
-- prefix []