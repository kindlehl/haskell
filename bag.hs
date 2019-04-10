module HW1types where
 
import Data.List (nub,sort)
 
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
bag1 = [(5,1),(7,3),(2,1),(3,2),(8,1)]
bag2 = [(5,1),(3,6),(1,0),(8,7),(4,1)]
 
ins :: Eq a  => a -> Bag a -> Bag a
ins x []     = [(x,1)]
ins x (y:ys) = if x == fst y
               then (x,succ(snd y)):ys
               else y : (ins x ys)
 
del :: Eq a  => a -> Bag a -> Bag a
del a []     = []
del a (x:xs) = if a == fst x
               then (a,pred(snd x)):xs
               else x : (del a xs)

build_bag :: Eq a => [a] -> Bag a -> Bag a
build_bag [] baggy = baggy
build_bag (x:xs) baggy = ins x (build_bag xs baggy)

bag :: Eq a => [a] -> Bag a
bag xs = build_bag xs []
