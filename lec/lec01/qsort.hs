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

-- fold right 
foldright :: (a-> b -> b) -> b -> [a] -> b
foldright f e [] = e
foldright f e (x: xs) = f x (foldright f e xs)

-- sum agian 
sum'' :: [Int] -> Int
sum'' l = foldright (+) 0 l 


-- map 
map' :: (a-> b )->  [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- map with fold right 
map'' :: (a-> b )->  [a] -> [b]
map'' f = foldright ( (:). f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x: xs) = if f x 
                        then x : filter' f xs
                        else filter' f xs  


-- filter with fold right 
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f  = foldright ((++).(\x-> if (f x) then [x] else [] )) []
-- filter' f (x:xs)= foldright ((:). (\x ->  if (f x) then [x] else [] )) []

-- foldleft 
foldleft :: (b -> a -> b ) -> b -> [a] -> b 
foldleft f e [] = e
foldleft f e (x: xs) = foldleft f (f e x)  xs 


-- reverse 
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++  [x]


-- reverse using foldright 
reverse'' :: [a] -> [a]
reverse'' = foldright (\a tail -> tail ++ [a]) [] 

-- performance reverse 
reverse''' :: [a] -> [a]
reverse''' l = rev l [] 
    where 
        rev [] ys = ys
        -- rev (x:xs) ys = rev xs ys:x
        rev (x:xs) ys =  rev xs (x : ys )


reverse'''' :: [a] -> [a]
reverse''''  = foldleft (\xs x -> x:xs) []

-- units 
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine","ten"]
teens =
    ["ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", "seventeen", "eighteen",
    "nineteen"]
tens =
    ["twenty", "thirty", "fourty", "fifty", "sixty",
    "seventy", "eighty", "ninety"]
convert n = units !! n 

digits2 :: Int -> (Int , Int)
digits2 n = (n `div` 10 , n `mod` 10)

-- combine2 (t, u)
--     | t == 0 = convert u
-- | t == 1 = teens !! u 
-- | u == 0 = tens !! (t -2)
-- | otherwise = tens !! (t-2)  ++ "-" ++ convert u

-- type String [Char]
combine2 :: (Int, Int) -> String
combine2 (t, u)
    | t == 0 = convert u
    | t == 1 = teens !! u
    | t > 1 && u == 0 = tens !! (t-2)
    | t > 1 && u /= 0 = tens !! (t-2)  ++ "-" ++ convert u