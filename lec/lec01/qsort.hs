qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    qsort smaller ++ [x] ++ qsort larger 
    where 
        smaller = filter (\a -> a <= x) xs
        larger  = filter (\a -> a > x)  xs


qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = 
    let 
        smaller = filter (<= x) xs
        larger  = filter (> x)  xs in
    qsort' smaller ++ [x] ++ qsort' larger 
        
        
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum xs 
 