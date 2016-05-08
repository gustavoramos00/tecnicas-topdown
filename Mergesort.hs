module Mergesort where
    import DivideAndConquer

    merge :: (Ord a) => [a] -> [a] -> [a]

    merge [] b = b
    merge a [] = a
    merge a@(x:xs) b@(y:ys)
        | x<=y = x:merge xs b
        | otherwise = y:merge a ys

    msort xs = divideAndConquer ind id divide combine xs
        where 
            ind xs = length xs <= 1
            divide xs = let n = length xs `div` 2
                in [take n xs, drop n xs]
            combine _ [l1,l2] = merge l1 l2