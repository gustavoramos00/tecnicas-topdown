module Graph(Graph,mkGraph,adjacent,nodes,edgesU,edgesD,edgeIn,
                   weight) where
import Data.Array

mkGraph :: (Eq w) => (Ix n,Num w) => Bool -> (n,n) -> [(n,n,w)] -> (Graph n w)
adjacent :: (Eq w) => (Ix n,Num w) => (Graph n w) -> n -> [n]
nodes :: (Eq w) => (Ix n,Num w) => (Graph n w) -> [n]
edgesU :: (Eq w) => (Ix n,Num w) => (Graph n w) -> [(n,n,w)]
edgesD :: (Eq w) => (Ix n,Num w) => (Graph n w) -> [(n,n,w)]
edgeIn :: (Eq w) => (Ix n,Num w) => (Graph n w) -> (n,n) -> Bool
weight :: (Eq w) => (Ix n,Num w) => n -> n -> (Graph n w) -> w

{-- Pointer representation --
data Graph n w = Vertex n [((Graph n w),w)] 

graphPTR = v1                   -- Figure 7.1 (c)
     where 
       v1 = Vertex 1 [(v2,12),(v3,34),(v5,78)]
       v2 = Vertex 2 [(v1,12),(v4,55),(v5,32)]
       v3 = Vertex 3 [(v1,34),(v4,61),(v5,44)]
       v4 = Vertex 4 [(v2,55),(v3,61),(v5,93)]
       v5 = Vertex 5 [(v1,78),(v2,32),(v3,44),(v4,93)]

-- End of Pointer representation --}

{-- Adjacency list representation --

type Graph n w = Array n [(n,w)]

mkGraph dir bnds es =
    accumArray (\xs x -> x:xs) [] bnds 
               ([(x1,(x2,w)) | (x1,x2,w) <- es] ++
                if dir then []
                else [(x2,(x1,w))|(x1,x2,w)<-es,x1/=x2])

adjacent g v    = map fst (g!v)

nodes g         = indices g

edgeIn g (x,y)  = elem y (adjacent g x)

weight x y g    = head [ c | (a,c)<-g!x , a==y]

edgesD g        = [(v1,v2,w) | v1<- nodes g , (v2,w) <-g!v1] 

edgesU g        = [(v1,v2,w) | v1<- nodes g , (v2,w) <-g!v1 , v1 < v2] 

graphAL = array (1,5) [(1,[(2,12),(3,34),(5,78)]),
                       (2,[(1,12),(4,55),(5,32)]),
                       (3,[(1,34),(4,61),(5,44)]),
                       (4,[(2,55),(3,61),(5,93)]),
                       (5,[(1,78),(2,32),(3,44),(4,93)])]

-- same graph but created with mkGraph but different ordering of arcs
graphAL' = mkGraph False (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]
                          
-- End of Adjacency list representation --}

{-- Adjacency matrix representation --}
type Graph n w = Array (n,n) (Maybe w)
            
mkGraph dir bnds@(l,u) es 
    = emptyArray // ([((x1,x2),Just w) |(x1,x2,w)<-es] ++
                     if dir then []
                     else [((x2,x1),Just w) |(x1,x2,w)<-es,x1/=x2])
      where
      emptyArray
          = array ((l,l),(u,u)) [((x1,x2),Nothing) | x1 <- range bnds, 
                                                     x2 <- range bnds]

adjacent g v1 = [ v2 | v2 <-nodes g,(g!(v1,v2))/= Nothing]

nodes g       = range (l,u) where ((l,_),(u,_)) = bounds g

edgeIn g (x,y)= (g!(x,y)) /= Nothing

weight x y g  = w where (Just w) = g!(x,y)

edgesD g       = [(v1,v2,unwrap(g!(v1,v2))) 
                      | v1 <-nodes g, v2 <- nodes g,
                        edgeIn g (v1,v2)]
    where unwrap (Just w) = w

edgesU g       = [(v1,v2,unwrap(g!(v1,v2)))
                   | v1 <-nodes g, v2 <- range (v1,u),
                     edgeIn g (v1,v2)]
    where (_,(u,_)) = bounds g          
          unwrap (Just w) = w

graphAM = mkGraph True (1,5) [(1,2,10),(1,3,20),(2,4,30),(3,4,40),(4,5,50)]

{-- End of Adjacency matrix representation --}

{-   Examples of evaluations and results

? graphAL
array (1, 5) [(1, [(2, 12), (3, 34), (5, 78)]), (2, [(1, 12), (4, 55), (5, 32)]), (3, [(1, 34), (4, 61), (5, 44)]), (4, [(2, 55), (3, 61), (5, 93)]), (5, [(1, 78), (2, 32), (3, 44), (4, 93)])]
? mkGraph (1,5) [(1,2,10),(1,3,20),(2,4,30),(3,4,40),(4,5,50)]
array (1, 5) [(1, [(3, 20), (2, 10)]), (2, [(4, 30)]), (3, [(4, 40)]), (4, [(5, 50)]), (5, [])]
? adjacent graphAL 4
[2, 3, 5]
? nodes graphAL
[1, 2, 3, 4, 5]
? edgesD graphAL
[(1, 2, 12), (1, 3, 34), (1, 5, 78), (2, 1, 12), (2, 4, 55), (2, 5, 32), (3, 1, 34), (3, 4, 61), (3, 5, 44), (4, 2, 55), (4, 3, 61), (4, 5, 93), (5, 1, 78), (5, 2, 32), (5, 3, 44), (5, 4, 93)]
? edgesU graphAL
[(1, 2, 12), (1, 3, 34), (1, 5, 78), (2, 4, 55), (2, 5, 32), (3, 4, 61), (3, 5, 44), (4, 5, 93)]
? edgeIn graphAL (3,2)
False
? edgeIn graphAL (3,4)
True
? weight 3 4 graphAL
61

? graphAM
array ((1, 1), (5, 5)) [((1, 1), Nothing), ((1, 2), Just 10), ((1, 3), Just 20), ((1, 4), Nothing), ((1, 5), Nothing), ((2, 1), Nothing), ((2, 2), Nothing), ((2, 3), Nothing), ((2, 4), Just 30), ((2, 5), Nothing), ((3, 1), Nothing), ((3, 2), Nothing), ((3, 3), Nothing), ((3, 4), Just 40), ((3, 5), Nothing), ((4, 1), Nothing), ((4, 2), Nothing), ((4, 3), Nothing), ((4, 4), Nothing), ((4, 5), Just 50), ((5, 1), Nothing), ((5, 2), Nothing), ((5, 3), Nothing), ((5, 4), Nothing), ((5, 5), Nothing)]
? adjacent graphAM 4
[5]
? nodes graphAM
[1, 2, 3, 4, 5]
? edgesD graphAM
[(1, 2, 10), (1, 3, 20), (2, 4, 30), (3, 4, 40), (4, 5, 50)]
? edgesU graphAM
[(1, 2, 10), (1, 3, 20), (2, 4, 30), (3, 4, 40), (4, 5, 50)]
? edgeIn graphAM (3,2)
False
? edgeIn graphAM (3,4)
True
? weight 3 4 graphAM
40

-}
