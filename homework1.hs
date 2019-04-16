module HW1types where

import Data.List (nub,sort)


-- Team Member
-- ID: 933620525 Name: Yeojin Kim
-- ID: 933612315 Name: Kuan Lun Tseng
-- ID: 933661736 Name: Xiangtian Xue
-- ID: 932545221 Name: Hunter Lannon
-- ID: 931932335 Name: Curt Haldorson

---------------------------------------------- Question 1

type Bag a = [(a,Int)]
nullList  = []
list = [2,3,3,5,7,7,7,8]
bag1 :: Bag Int
bag2 :: Bag Int
bag1 = [(5,1),(7,3),(2,1),(3,2),(8,1)]
bag2 = [(5,1),(3,6),(1,0),(8,7),(4,1)]

{-
ins :: Eq a => a -> Bag a -> Bag a
ins a []     = [(a, 1)]
ins a (x:xs) = if (a == fst x) 
			   then (fst x, succ (snd x)):xs 
			   else x : (ins a xs)
-}

ins :: Eq a  => a -> Bag a -> Bag a
ins x []     = [(x,1)]
ins x (y:ys)  | x == fst y = (x,succ(snd y)):ys
              | otherwise  = y : (ins x ys)

{-
del :: Eq a  => a -> Bag a -> Bag a
del a []     = []
del a (x:xs) = if a == fst x
               then filter (\y -> (snd y) > 0) ((a,pred(snd x)):xs)
               else x : (del a xs)
-}

del :: Eq a  => a -> Bag a -> Bag a
del a []     = []
del a (x:xs) | a == fst x = filter (\y -> (snd y) > 0) ((a,pred(snd x)):xs)
             | otherwise  = x : (del a xs)

{-
build_bag :: Eq a => [a] -> Bag a -> Bag a
build_bag [] baggy = baggy
build_bag (x:xs) baggy = ins x (build_bag xs baggy)
bag :: Eq a => [a] -> Bag a
bag xs = build_bag xs []
-}

bag :: Eq a => [a] -> Bag a
bag []     = []
bag (x:xs) = ins x (bag xs)

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag xs [] = False
subbag [] ys = True
subbag (x:xs) ys = check x ys && subbag xs ys

check x []     = False
check x (y:ys) | x == y = True
               | otherwise = check x ys

isbag :: Eq a   => Bag a -> Bag a -> Bag a
isbag [] ys     = []
isbag (x:xs) ys | check x ys = x : isbag xs ys
                | otherwise  = isbag xs ys


{- | Testing subbag, isbag
>>> subbag [(7,3),(2,1)] [(5,1),(7,3),(2,1),(3,2),(8,1)]
True
>>> isbag [(2,1),(3,2),(8,3)] [(5,1),(7,3),(2,1),(3,2),(8,1)]
[(2,1),(3,2)]
>>> size [(2,1),(3,2),(8,3)]
6
>>> size [(5,1),(7,3),(2,1),(3,2),(8,1)]
8
-}

size :: Bag a -> Int
size [] = 0
size (x:xs) = snd x + size xs

--Graphs

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
---------------------------------------------- Question 2
g :: Graph
g = [(1,2), (1,3), (2,3), (2,4), (3,4)]

h :: Graph
h = [(1,2), (1,3), (2,1), (3,2), (4,4)]

{- | Testing graphs
>>> nodes h
[1,2,3,4]
>>> nodes [(1,9), (2,27)]
[1,2,9,27]
>>> suc 1 [(1,9), (2,27), (1,29)]
[9,29]
>>> suc 2 [(1,9), (2,27), (1,29)]
[27]
>>> detach 2 [(1,9), (2,27), (1,29)]
[(1,9),(1,29)]
>>> detach 1 [(1,1), (1,7), (1,9), (2,27), (1,29)]
[(2,27)]
>>> cyc 4
[(1,2),(2,3),(3,4),(4,1)]
>>> cyc 1
[(1,1)]
>>> cyc 2
[(1,2),(2,1)]
-}

fromEtoN:: Edge -> [Node]
fromEtoN (x,y) = [x,y]

fromGtoN:: Graph -> [Node]
fromGtoN [(x,y)] = [x,y]
fromGtoN (x:xs) = fromEtoN (x) ++ fromGtoN xs 

graph1 :: Graph
graph2 :: Graph
graph1 = [(1,2),(1,3),(2,3),(2,4),(3,4)]
graph2 = [(1,2),(1,3),(2,1),(3,2),(4,4)]

norm :: Ord a => [a] -> [a]
norm = sort . nub
                             
nodes :: Graph -> [Node]
nodes [] = []
nodes (x:xs) = norm ([fst x, snd x] ++ nodes xs)

suc :: Node -> Graph -> [Node]
suc x [] = []
suc x ((y1,y2):ys) | x == y1  = [y2] ++ suc x ys
                   | otherwise  = suc x ys

detach :: Node -> Graph -> Graph
detach x [] = []
detach x ((y1,y2):ys) | x == y1 = detach x ys
                      | x == y2   = detach x ys
                      | otherwise = (y1,y2) : detach x ys

bc :: Int -> Graph 
bc 1 = []
bc x = (bc (x-1) ++ [(x-1, x)])

cyc x = bc x ++ [(x,1)]

---------------------------------------------- Question 3

type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
            deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

point = Pt (1,1)
circle = Circle (2,2) 2
rectangle = Rect (3,3) 3 3
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

{- | Tests for bbox
>>> map width [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
[0,6,7]
>>> map bbox [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
[((4,4),(4,4)),((2,2),(8,8)),((3,3),(10,5))]
>>> map minX [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
[4,2,3]
-}


{- | Testing bbox functions
>>> move (f!!1) (1,2)
Circle (6,7) 3
>>> move (f!!0) (1,2)
Pt (5,6)
>>> move (f!!2) (1,2)
Rect (4,5) 7 2
>>> map minX f
[4,2,3]
>>> alignLeft [Pt (0,4),Pt (4,4), Circle (4, 4) 1]
[Pt (0,4),Pt (0,4),Circle (1,4) 1]
>>> inside (f!!0) (f!!1)
True
-}

width :: Shape -> Length
width (Pt p)          = 0
width (Circle p l)    = l*2
width (Rect p l1 l2)  = l1

bbox :: Shape -> BBox
bbox (Pt p)           = (p, p)
bbox (Circle p l)     = ((fst p - l, snd p - l),((fst p + l),(snd p + l)))
bbox (Rect p l1 l2)   = (p, ((fst p + l1),(snd p + l2)))

minX :: Shape -> Number
minX (Pt p) = fst p
minX (Rect p l1 l2) = fst p
minX (Circle p l) = fst p - l

move :: Shape -> Point -> Shape
move (Pt p) v = (Pt (addPt p v))
move (Rect p l1 l2) v = (Rect (addPt p v) l1 l2)
move (Circle p l) v  = (Circle (addPt p v) l)

addPt :: Point -> Point -> Point
addPt (x1,y1) (x2,y2) = (x1+x2,y1+y2)

alignLeft :: Figure -> Figure
alignLeft x = map (moveToX (minimum(map minX x))) x

moveToX :: Number -> Shape -> Shape
moveToX n (Pt (x,y))             = (Pt (n, y))
moveToX n (Circle (x,y) l)       = (Circle (n+l,y) l)
moveToX n (Rect (x,y) l1 l2)     = (Rect (n,y) l1 l2)

inside :: Shape -> Shape -> Bool
inside a b = (inCheck (bbox a) (bbox b))

inCheck :: BBox -> BBox -> Bool
inCheck ((a,b),(c,d)) ((e,f),(g,h)) = (lineCheck [e, f, c, d] [a, b, g, h])

lineCheck :: [Int] -> [Int] -> Bool
lineCheck [] [] = True
lineCheck (x:xs) (y:ys) | x <= y && lineCheck xs ys = True
                        | otherwise = False