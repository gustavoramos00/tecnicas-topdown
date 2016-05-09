module S8_1 where

divideAndConquer :: ( p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s)
      -> p -> s
divideAndConquer ind solve divide combine initPb 
    = dc' initPb
    where dc' pb
              | ind pb  = solve pb
              | otherwise = combine pb (map dc' (divide pb))

msort xs = divideAndConquer ind id divide combine xs
    where ind xs            = length xs <= 1
          divide xs         = let n = length xs `div` 2
                                in [take n xs , drop n xs]
          combine _ [l1,l2] = merge l1 l2

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys) | (x<=y)    = x : (merge xs b)
                        | otherwise = y : (merge a ys)

qsort xs = divideAndConquer ind id divide combine xs
    where ind xs                = length xs <= 1
          divide (x:xs)         = [[ y | y<-xs, y<=x],
                                   [ y | y<-xs, y>x] ]
          combine (x:_) [l1,l2] = l1 ++ [x] ++ l2

l = [3,1,4,1,5,9,2]

{- Examples of evaluations and results 

? msort l
[1, 1, 2, 3, 4, 5, 9]
? qsort l
[1, 1, 2, 3, 4, 5, 9]

-}
