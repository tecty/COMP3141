bar :: [Int] -> [Int]
bar = undefined

prop_1 :: [Int] -> Bool
prop_1 xs = bar (bar xs) == xs

prop_2 :: [Int] -> Bool
prop_2 xs = length xs == length (bar xs)

prop_3 :: [Int] -> (Int -> Int) -> Bool
prop_3 xs f = bar (map f xs) == map f (bar xs)