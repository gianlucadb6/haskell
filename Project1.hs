module Project1 where
import Data.List
import Data.Tuple

-- triples returns the number of items in a list that are divisible by 3.
--
-- > triples [1,2,3,4,5,6]
-- 2
-- > triples [3,33,333]
-- 3
-- > triples [0,1,4]
-- 1

triples :: [Integer] -> Integer
--0 case
triples [] = 0
--if divisible by 3, count + recursive call
triples (x:xs) = if x `mod` 3 == 0 then 1 + triples(xs) else 0 + triples(xs)




-- The hailstone sequence takes a positive number n and repeatedly applies
-- this transformation: if n is even, it divides n by 2; otherwise, it
-- multiplies n by 3 and adds one. The sequence ends when it reaches 1.
--
-- hailstone returns the complete sequence beginning with a particular number.
-- You may assume that the number is positive.
--
-- > hailstone 4
-- [4,2,1]
-- > hailstone 6
-- [6,3,10,5,16,8,4,2,1]
-- > hailstone 7
-- [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]

hailstone :: Integer -> [Integer]
--two base cases, for zero return nothing and 1 just a list containing itself
hailstone 0 = []
hailstone 1 = [1]
--any other number n should apply the transformation
--even = n/2, odd = (n*3)+1
hailstone n 
        |(n `mod` 2 == 0) = n : hailstone(n`div`2)  
	|otherwise = n : hailstone((n*3)+1)


		


data Point = Pt Double Double deriving (Show, Eq)

-- The centroid of a list of points is a point whose x (and y) coordinates are
-- the means of the x (and y) coordinates of the points in the list.
--
-- You may assume the list contains at least one point.
--
-- > centroid [Pt 1 1, Pt 2 2]
-- Pt 1.5 1.5
-- > centroid [Pt (-1.5) 0, Pt 3 2, Pt 0 1]
-- 58  Pt 0.5 1.0

centroid :: [Point] -> Point
centroid [] = Pt 0.0 0.0
centroid [x] = x
centroid (x:xs) = Pt ((addXs (x:xs)) / (lengthN(x:xs))) ((addYs (x:xs)) / (lengthN(x:xs)))
--length of list function
lengthN :: [Point] -> Double
lengthN [] = 0
lengthN (x:xs) = 1 + lengthN xs 
--adding x-comps
addXs :: [Point] -> Double
addXs[] = 0.0
addXs (x:xs) = fstN(x) + addXs(xs)
--adding y-comps
addYs :: [Point] -> Double
addYs[] = 0.0
addYs (x:xs) = sndN(x) + addYs(xs)
--extracting x-comp
fstN :: Point -> Double
fstN (Pt x y) = x
--extracting y-comp
sndN :: Point -> Double
sndN (Pt x y) = y





data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)


-- mirror returns a tree with the same shape and contents as its argument, but
-- flipped left for right.
--
-- > mirror (Bin (Bin Tip 1 (Bin Tip 2 Tip)) 3 Tip)
-- Bin Tip 3 (Bin (Bin Tip 2 Tip) 1 Tip)

mirror :: Tree a -> Tree a
--empty tree
mirror Tip = Tip
--tree with one node
mirror (Bin Tip a Tip) = (Bin Tip a Tip)
--tree with #nodes > 1
mirror (Bin left a right) = flipTree(Bin left a right)

--helper function to mirror a tree
flipTree :: Tree a -> Tree a
flipTree Tip = Tip
flipTree (Bin left a right) = (Bin (flipTree right) a (flipTree left))

-- In a strictly binary tree, each node has either 0 children or 2 children.
--
-- > strict (Bin Tip 1 Tip)
-- True
-- > strict (Bin Tip 1 (Bin Tip 2 Tip))
-- False

strict :: Tree a -> Bool
--empty
strict Tip = True
--node with no children
strict (Bin Tip a Tip) = True
--next two cases deal with a node having one child on either side
strict (Bin Tip a right) = False 
strict (Bin left a Tip) = False
--recursive call to check the children of a nodes children
strict (Bin left a right) = (strict left) && (strict right)


-- A tree is near balanced if the left and right sub-trees of each node differ
-- in height by at most 1. 
--
-- > near_balanced (Bin Tip 1 (Bin Tip 2 Tip))
-- True
-- > near_balanced (Bin Tip 1 (Bin Tip 2 (Bin Tip 3 Tip)))
-- False
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin True 3 Tip)))
-- True
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin (Bin Tip 4 Tip) 3 Tip)))
-- False

near_balanced :: Tree a -> Bool
--empty tree
near_balanced Tip = True
--one node
near_balanced (Bin Tip a Tip) = True
--any other amount of nodes, make sure the max different in heights of 
--trees is no more than one
near_balanced (Bin left a right) = if (abs (treeHeight left - treeHeight right)) < 2 
						then True else False


--helper method to count height of trees
treeHeight :: Tree a -> Integer
--tip has height 0
treeHeight Tip = 0
--height at any node is 1 + max height of either subtree 
treeHeight (Bin left a right) = 1 + max (treeHeight left) (treeHeight right)





