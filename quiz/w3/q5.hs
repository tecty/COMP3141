foo :: [a] -> (a -> b) -> [b]
foo [] f = []
foo (x:xs) f = f x : foo xs f

prop_1 :: [Int] -> Bool
prop_1 xs = foo xs id == xs 

prop_2 :: [Int] -> (Int -> Int) -> (Int -> Int) -> Bool
prop_2 xs f g = foo (foo xs f) g == foo xs (g . f)