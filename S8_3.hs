module S8_3 where
-- to get access to the imported modules in Hugs do
-- :set -P../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts

import Data.Array
import PQueue

-- PRIORITY-FIRST FRAMEWORK

searchPfs             :: (Ord node) => (node -> [node]) -> (node -> Bool) 
                          -> node -> [(node,Int)]
searchPfs succ goal x = search' (enPQ x emptyPQ) 0
 where
   search' q  t
    | (pqEmpty q)      = []
    | goal (frontPQ q) = ((frontPQ q),t+1):(search' (dePQ q)(t+1))
    | otherwise        = let x = frontPQ q
                         in search' (foldr enPQ (dePQ q) (succ x)) (t+1)

-- copied from S8_2

type Position        = (Int,Int)
type Board           = Array Int Position
data Boards          = BDS [Board] deriving Show

g8T :: Board
g8T = array (0,8) [(0,(2,2)),(1,(1,1)),(2,(1,2)),
                   (3,(1,3)),(4,(2,3)),(5,(3,3)),
                   (6,(3,2)),(7,(3,1)),(8,(2,1))]

s8T :: Board
s8T = array (0,8) [(0,(2,2)),(1,(1,2)),(2,(1,1)),
                   (3,(3,3)),(4,(2,1)),(5,(3,2)),
                   (6,(1,3)),(7,(3,1)),(8,(2,3))]

mandist :: Position -> Position -> Int
mandist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

allMoves   :: Board -> [Board]
allMoves b = [ b//[(0,b!i),(i,b!0)] 
               | i<-[1..8], (mandist (b!0) (b!i) == 1)]

succ8Tile     :: Boards -> [Boards]
succ8Tile (BDS(n@(b:bs)))  
              = filter (notIn bs) [ BDS(b':n) | b' <- allMoves b]
  where
    notIn bs (BDS(b:_)) 
              = not (elem (elems b) (map elems bs))

goal8Tile            :: Boards -> Bool
goal8Tile (BDS(n:_)) = (elems n == elems g8T)

-- section 8.3.2

heur1   :: Board  -> Int
heur1 b = sum [ mandist (b!i) (g8T!i) | i<-[0..8]]

{--}
-- Need to define equality of Boards in the priority queue
--  before defining order
instance Eq Boards
    where BDS(b1:_) == BDS(b2:_) = heur1 b1 == heur1 b2

instance Ord Boards
    where BDS (b1:_) <= BDS (b2:_) = heur1 b1 <= heur1 b2
{--}

-- second heuristic

outseq   :: Board -> Int
outseq b = sum [score (b!i) ((b!(i+1)))|i<-[1..7]]+score (b!8) (b!1)

heur2 :: Board -> Int
heur2 b = (heur1 b) + 3*(outseq b)

score :: Position -> Position -> Int
score (2,2) _      = 1

score (1,3) (2,3)  = 0
score (2,3) (3,3)  = 0
score (3,3) (3,2)  = 0
score (3,2) (3,1)  = 0
score (3,1) (2,1)  = 0
score (2,1) (1,1)  = 0
score (1,1) (1,2)  = 0
score (1,2) (1,3)  = 0

score _ _          = 2

{--
-- Need to define equality of Boards in the priority queue
--  before defining order
instance Eq Boards
  where BDS(b1:_) == BDS(b2:_) = heur2 b1 == heur2 b2

instance Ord Boards
  where BDS (b1:_) <  BDS (b2:_) = heur2 b1 <  heur2 b2
        BDS (b1:_) <= BDS (b2:_) = heur2 b1 <= heur2 b2
 --}


stats  = (length ls , t)
 where
   (((BDS ls),t):_) = searchPfs succ8Tile goal8Tile (BDS [s8T])

{- Examples of evaluations and results 
-- with heur1 36 nodes are explored and sol length = 25
? pfs8Tile
S8_3> stats
(25, 36)
(169055 reductions, 260639 cells)
-- with heur2 68 nodes are explored and sol length = 19
? pfs8Tile
S8_3> stats
(19, 68)
(616341 reductions, 939828 cells, 1 garbage collection)
-}
