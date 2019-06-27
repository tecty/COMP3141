baz :: [Integer] -> Integer
baz []     = 0
baz (x:xs) = 1 + baz xs

prop_1 :: [Integer] -> [Integer] -> Bool
prop_1 xs ys = baz xs + baz ys == baz (xs ++ ys)

prop_2 :: [Integer] -> Bool
prop_2 xs = baz xs == baz (reverse xs) 

prop_3 :: Integer -> [Integer] -> Bool 
prop_3 x xs = baz (x:xs) - x == baz xs