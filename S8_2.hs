module S8_2 where
-- to get access to the imported modules in Hugs do
-- :set -P../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts

import Stack
import Data.Array

-- Section 8.2.1

type Position        = (Int,Int)
type Board           = Array Int Position
data Boards          = BDS [Board] deriving Eq

g8T :: Board
g8T = array (0,8) [(0,(2,2)),(1,(1,3)),(2,(2,3)),
                   (3,(3,3)),(4,(3,2)),(5,(3,1)),
                   (6,(2,1)),(7,(1,1)),(8,(1,2))]

s8T :: Board 
s8T = array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,3)),
                   (3,(3,3)),(4,(3,2)),(5,(1,2)),
                   (6,(2,3)),(7,(2,1)),(8,(3,1))]

mandist                 :: Position -> Position -> Int
mandist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

allMoves   :: Board -> [Board]
allMoves b = [ b//[(0,b!i),(i,b!0)] 
               | i<-[1..8], mandist (b!0) (b!i) == 1]

succ8Tile     :: Boards -> [Boards]
succ8Tile (BDS(n@(b:bs)))  
              = filter (notIn bs) [ BDS(b':n) | b' <- allMoves b]
  where
    notIn bs (BDS(b:_)) = not (elem (elems b) (map elems bs))

-- Section 8.2.2

searchDfs:: (Eq node) => (node -> [node]) -> (node -> Bool) 
                                          -> node -> [node]
searchDfs succ goal x = search' (push x emptyStack) 
 where
   search' s  
    | stackEmpty s = [] 
    | goal (top s) = top s : search' (pop s)
    | otherwise    = let x = top s
                     in search' (foldr push (pop s) (succ x))

-- Section 8.2.3

goal8Tile            :: Boards -> Bool
goal8Tile (BDS (n:_)) = elems n == elems g8T

dfs8Tile  :: [[Position]]
dfs8Tile  = map elems ls
 where ((BDS ls):_) = searchDfs succ8Tile goal8Tile (BDS [s8T])

-- Section 8.2.4

type Column     = Int
type Row        = Int
type SolNQ      = [(Column,Row)]

type NodeNQ     = (Column,Column,SolNQ)

valid              :: SolNQ -> (Column,Row) -> Bool
valid psol (c,r)   = and (map test psol)
    where test (c',r') = and [c'+r'/=c+r,c'-r'/=c-r,r'/=r]

succNq :: NodeNQ -> [NodeNQ]
succNq (c,n,psol)
    = [(c+1,n,psol++[(c,r)]) | r<-[1..n] , valid psol (c,r)]

goalNq :: NodeNQ -> Bool
goalNq (c,n,psol) = c > n

firstNq   :: Column -> SolNQ
firstNq n = s
    where ((_,_,s):_) = searchDfs succNq goalNq (1,n,[])

countNq   :: Column -> Int
countNq n = length (searchDfs succNq goalNq (1,n,[]))

-- Section 8.2.5

type Weight         = Int
type Value          = Float
type Object         = (Weight,Value)
type SolKnp         = [Object]
type NodeKnp        = (Value,Weight,Weight,[Object],SolKnp)

succKnp            :: NodeKnp -> [NodeKnp]
succKnp (v,w,limit,objects,psol)
                   =[( v+v',
                       w+w',limit,
                       [ o | o@(w'',_) <- objects,(w''>=w')], 
                       (w',v'):psol )
                     | (w',v') <- objects , w+w' <= limit]

goalKnp (_,w,limit,((w',_):_),_) = (w+w'>limit)

knapsack :: [Object] -> Weight -> (SolKnp,Value)
knapsack objects limit = (sol,v) 
    where (v,_,_,_,sol) = maximum (searchDfs succKnp goalKnp  
                                             (0,0,limit,qsort objects,[]))
                          
-- naive implementation of quicksort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (pivot:rest) = qsort lower ++ [pivot] ++ qsort upper
    where lower = [ x | x <- rest, x<= pivot]
          upper = [ x | x <- rest, x > pivot] 

--PUT WEIGHT FIRST FIRST
-- Brassard and Bratley p. 307
v = knapsack [(2,3),(3,5),(4,6),(5,10)] 8



--Example in the book
k = knapsack [(2,3),(3,5),(5,6)] 10

-- GIVES : ([(3,5), (3,5), (2,3), (2,3)],16)

k' = knapsack [(8,15),(15,10),(3,6),(6,13),
                     (2,4),(4,8),(5,6),(7,7)] 35

--GIVES : ([(13,6), (13,6), (13,6), (13,6), (13,6), (6,3), (4,2)],75)
-- (183399 reductions, 441898 cells, 5 garbage collections in old version)

--New Example in book
k''= knapsack [(2,2.8),(3,4.4),(5,6.1)] 10
-- GIVES : ([(3,4.4), (3,4.4), (2,2.8), (2,2.8)],14.4)


-- FOR DISPLAYING FIGURE ONLY

knp objects limit = [ (sol,v) | (v,w,_,_,sol) <- searchDfs succKnp goalKnp  
                                                  (0,0,limit,qsort objects,[]),
                                 ((w==10)||(w==9)||(w==8)) ]
test = knp [(2,2.8),(3,4.4),(5,6.1)] 10


{- Examples of evaluations and results 

? dfs8Tile  -- never terminated.... 
{Interrupted!}
? firstNq 8
[(1, 1), (2, 5), (3, 8), (4, 6), (5, 3), (6, 7), (7, 2), (8, 4)]
? countNq 8
92
? k
([(3, 5.0), (3, 5.0), (2, 3.0), (2, 3.0)], 16.0)
? k'
(
ERROR: Control stack overflow
? k''
([(3, 4.4), (3, 4.4), (2, 2.8), (2, 2.8)], 14.4)
? test
[([(2, 2.8), (2, 2.8), (2, 2.8), (2, 2.8), (2, 2.8)], 14.0), ([(3, 4.4), (2, 2.8), (2, 2.8), (2, 2.8)], 12.8), ([(3, 4.4), (3, 4.4), (2, 2.8), (2, 2.8)], 14.4), ([(5, 6.1), (2, 2.8), (2, 2.8)], 11.7), ([(3, 4.4), (3, 4.4), (2, 2.8)], 11.6), ([(5, 6.1), (3, 4.4), (2, 2.8)], 13.3), ([(3, 4.4), (3, 4.4), (3, 4.4)], 13.2), ([(5, 6.1), (3, 4.4)], 10.5), ([(5, 6.1), (5, 6.1)], 12.2)]

-}
