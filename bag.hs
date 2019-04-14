module HW1types where

import Data.List (nub,sort)
import qualified Data.Set as Set

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
---------------------------------------------- Bags

type Bag a = [(a,Int)]

norm :: Ord a => [a] -> [a]
norm = sort . nub

nullList  = []
list = [2,3,3,5,7,7,7,8]
bag1 :: Bag Int
bag2 :: Bag Int

{- | Insert item into bag
>>> ins 5 bag1
[(5,2),(7,3)]

>>> ins 5 []
[(5,1)]
-}

bag1 = [(5,1),(7,3),(2,1),(3,2),(8,1)]
bag2 = [(5,1),(3,6),(1,0),(8,7),(4,1)]

---------------------------------------------- 1-a
ins :: Eq a  => a -> Bag a -> Bag a
ins x []     = [(x,1)]
ins x (y:ys) | x == fst y = (x,succ(snd y)):ys
			 | otherwise  = y : (ins x ys)

{- | Delete item from bag
>>> del 5 bag1
[(7,3)]

>>> del 5 []
[]
-}

-- 1-b
del :: Eq a  => a -> Bag a -> Bag a
del a []     = []
del a (x:xs) | a == fst x = filter (\y -> (snd y) > 0) ((a,pred(snd x)):xs)
             | otherwise  = x : (del a xs)

{- | Build bag from list
>>> bag ["Toyota", "Fiat", "Honda", "Fiat"]
[("Fiat",2),("Honda",1),("Toyota",1)]

>>> bag []
[]
-}

build_bag :: Eq a => [a] -> Bag a -> Bag a
build_bag [] baggy = baggy
build_bag (x:xs) baggy = ins x (build_bag xs baggy)


---------------------------------------------- 1-c
bag :: Eq a => [a] -> Bag a
bag []     = []
bag xs = build_bag xs []

---------------------------------------------- 1-d
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag xs [] = False
subbag [] ys = True
subbag (x:xs) ys = check x ys && subbag xs ys

check x []	   = False
check x (y:ys) | x == y = True
			   | otherwise = check x ys

---------------------------------------------- 1-e
isbag :: Eq a 	=> Bag a -> Bag a -> Bag a
isbag [] ys     = []
isbag (x:xs) ys | check x ys = x : isbag xs ys
				| otherwise  = isbag xs ys

---------------------------------------------- 1-f
size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size xs


---------------------------------------------- 2-a
g :: Graph
g = [(1,2), (1,3), (2,3), (2,4), (3,4)]

h :: Graph
h = [(1,2), (1,3), (2,1), (3,2), (4,4)]

fromEtoN:: Edge -> [Node]
fromEtoN (x,y) = [x,y]

fromGtoN:: Graph -> [Node]
fromGtoN [(x,y)] = [x,y]
fromGtoN (x:xs) = fromEtoN (x) ++ fromGtoN xs 

makeSet :: Ord a => [a] -> [a]
makeSet = makeSet' Set.empty where
makeSet' _ [] = []
makeSet' a (b : c) = if Set.member b a then makeSet' a c else b : makeSet' (Set.insert b a) c

nodes :: Graph -> [Node]
nodes graph = makeSet(fromGtoN graph)
               
---------------------------------------------- 2-b
successor:: Node -> Edge -> [Node]
successor z (x,y) = case z == x of 
                   True  -> [y]
                   False -> []

suc:: Node -> Graph -> [Node]
suc z [(x,y)] = successor z (x,y)
suc z (x:xs) = successor z (x) ++ suc z xs

---------------------------------------------- 2-c
remNode:: Node -> Edge -> [Edge]
remNode z (x,y) = if (z /= x && z /= y) then [(x,y)] else []

detach :: Node -> Graph -> Graph
detach z [(x,y)] = remNode z (x,y)
detach z (x:xs) = remNode z (x) ++ detach z xs

---------------------------------------------- 2-d
cyc::Int -> Graph
cyc z = zip [1 .. z-1][2 .. z] ++ [(z,1)]

------------------------------------------------ Shapes

type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
		   | Circle Point Length
		   | Rect 	Point Length Length
		     deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

point = Pt (1,1)
circle = Circle (2,2) 2
rectangle = Rect (3,3) 3 3
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

---------------------------------------------- 3-a
width :: Shape -> Length
width (Pt p) 			= 0
width (Circle p l)		= l*2
width (Rect p l1 l2) 	= l1 * l2

---------------------------------------------- 3-b
bbox :: Shape -> BBox
bbox (Pt p) 		= (p, p)
bbox (Circle p l) 	= ((fst p - l, fst p - l),((snd p + l),(snd p + l)))
bbox (Rect p l1 l2)	= (p, ((fst p + l1),(snd p + l2)))

---------------------------------------------- 3-c
minX :: Shape -> Number
minX (Pt p)	= fst p
minX (Circle p l) | fst p - l <= snd p - l 	= fst p - l
		  | otherwise				= snd p - l
minX (Rect p l1 l2) | fst p <= snd p = fst p
		    | otherwise		 = snd p
					
---------------------------------------------- 3-d
move :: Shape -> Point -> Shape
					
					
					
addPt :: Point -> Point -> Point

