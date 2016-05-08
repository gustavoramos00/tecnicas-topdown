module DivideAndConquer where
    --ind :: p -> Bool
    --solve :: p -> s
    --divide :: p -: [s]
    --combine :: p -> [s] -> s

    divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s

    divideAndConquer ind solve divide combine initPb =  dc' initPb
        where dc' pb
                | ind pb = solve pb
                | otherwise = combine pb (map dc' (divide pb))