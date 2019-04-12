module HW1types where
 
import Data.List (nub,sort)
import qualified Data.Set as Set

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
type Bag a = [(a,Int)]
 
norm :: Ord a => [a] -> [a]
norm = sort . nub
 
nullList  = []
list = [2,3,3,5,7,7,7,8]
bag1 :: Bag Int
bag2 :: Bag Int
bag1 = [(5,1),(7,3)]
bag2 = [(5,1),(3,6)]
 
{- | Insert item into bag
>>> ins 5 bag1
[(5,2),(7,3)]

>>> ins 5 []
[(5,1)]

-}
ins :: Eq a  => a -> Bag a -> Bag a
ins x []     = [(x,1)]
ins x (y:ys) = if x == fst y
               then (x,succ(snd y)):ys
               else y : (ins x ys)
{- | Delete item from bag
>>> del 5 bag1
[(7,3)]

>>> del 5 []
[]
-}

del :: Eq a  => a -> Bag a -> Bag a
del a []     = []
del a (x:xs) = if a == fst x
               then filter (\y -> (snd y) > 0) ((a,pred(snd x)):xs)
               else x : (del a xs)

{- | Build bag from list
>>> bag ["Toyota", "Fiat", "Honda", "Fiat"]
[("Fiat",2),("Honda",1),("Toyota",1)]

>>> bag []
[]
-}

build_bag :: Eq a => [a] -> Bag a -> Bag a
build_bag [] baggy = baggy
build_bag (x:xs) baggy = ins x (build_bag xs baggy)

bag :: Eq a => [a] -> Bag a
bag xs = build_bag xs []


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
