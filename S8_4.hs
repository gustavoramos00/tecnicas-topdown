module S8_4 where
-- to get access to the imported modules in Hugs do
-- :set -P../Chapter7:../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts

import PQueue
import Graph
import Data.List
import Data.Ix

searchGreedy :: (Ord node) => (node -> [node]) -> (node -> Bool) 
                              -> node -> [node]
searchGreedy succ goal x = (search' (enPQ x emptyPQ) )
 where
   search' q  
    | (pqEmpty q)      = [] 
    | goal (frontPQ q) = [frontPQ q]
    | otherwise        = let x = frontPQ q
                         in search' (foldr enPQ emptyPQ (succ x))


-- Section 8.4.2 SOLVING THE COINS PROBLEM
coins :: [Coin]
coins = [1,2,5,10,20,50,100]

type Coin            = Int
type SolChange       = [Coin]
type NodeChange      = (Int , SolChange)

succCoins       :: NodeChange -> [NodeChange]
succCoins (r,p) = [ (r-c,c:p) | c <- coins , r - c >=0 ]

goalCoins       :: NodeChange -> Bool
goalCoins (v,_) = v==0

change        :: Int -> SolChange
change amount = snd(head(searchGreedy succCoins goalCoins (amount,[])))

-- Section 8.4.3 Prim's minimum spanning tree

type Edge a b    = (a,a,b)

type NodeMst a b = (b , [a] ,[a], [Edge a b])

succMst :: (Eq b) => (Ix a,Num b) => (Graph a b) -> (NodeMst a b) 
                           -> [(NodeMst a b)]
succMst g (_,t,r,mst)
        = [(weight x y g, (y:t), delete y r, (x,y,weight x y g):mst)
              | x <- t , y <- r, edgeIn g (x,y)]

goalMst (_,_,[],_) = True
goalMst _          = False

prim g = sol
    where [(_,_,_,sol)] = searchGreedy (succMst g) goalMst
                                       (0,[n],ns,[])
          (n:ns) = nodes g

graphEx::(Graph Int Int)
graphEx = mkGraph True (1,5)
           [(1,2,12),(1,3,34),(1,5,78),(2,1,12),(2,5,32),(2,4,55),
            (3,1,34),(3,5,44),(3,4,61),(4,2,55),(4,3,61),(4,5,93),
            (5,1,78),(5,2,32),(5,3,44),(5,4,93)]

{- Examples of evaluations and results 

? change 199
[2, 2, 5, 20, 20, 50, 100]
? prim graphEx
[(2, 4, 55), (1, 3, 34), (2, 5, 32), (1, 2, 12)]

-}
