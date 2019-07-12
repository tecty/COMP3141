import Test.QuickCheck

d6 :: [Int]
d6 = [1,2,3,4,5,6]

twoD6 :: [(Int, Int)]
twoD6 = (,) <$> d6 <*> d6 
-- d6  >>= (\x -> d6 >>= \y -> pure (x, y))
    
game :: [(Int, Int)]
game = do 
    (d1, d2) <- twoD6
    if abs (d1 - d2) < 2 then do 
        d2'  <- d6
        pure (d1, d2')
    else 
        pure (d1, d2)
-- game = twoD6 >>= (\(d1, d2) -> 
--     if abs (d1 - d2) < 2 then 
--             d6 >>= \d2' -> pure (d1, d2')
--         else 
--             pure (d1, d2) 
--         )

scores :: [Int]
scores = fmap score game

score :: (Int, Int) -> Int 
score (d1, d2) = abs (d1 - d2)


students = [(5141448, "Huang"), (4444444, "happy"), (8888888, "hello")]


-- lookup :: [(a, b)] -> a -> Maybe b
findNames :: [Int] -> Maybe [String]
findNames [] = return []
findNames (z:zs) = do 
    n <- lookup z students
    ns <- findNames zs
    pure (n: ns)

-- findNames (z:zs) = lookup z students  >>=(\n -> 
--         findNames zs 
--             >>= (\ns -> 
--                 pure (n : ns)
--                 ) 
--     )


data Tree = Leaf 
    | Branch Int Tree Tree 
                deriving (Show, Eq)
instance Arbitrary Tree where
-- arbitrary :: Gen Tree
arbitrary = do 
    min <- arbitrary 
    Positive max' <- arbitrary
    pure searchTrees min (min + max')
    where 
        searchTrees :: Int -> Int -> Gen Tree 
        searchTrees min max 
            | min < max = oneof [ leafGen
                    ,branchGen
                ]
            | otherwise = leafGen
                where 
                    leafGen:: Gen Tree 
                    leafGen  = pure Leaf 
                    branchGen :: Gen Tree 
                    branchGen = do 
                        n <- choose (min, max)
                        pure Branch n <$> 
                            searchTrees min s(n -  1)  <*>
                            searchTrees n max

-- -- arbitrary :: Gen Tree
-- arbitrary = arbitrary >>= \min 
--     -> arbitrary >>= \(Positive max') 
--         -> searchTrees min (min + max')
--     where 
--         searchTrees :: Int -> Int -> Gen Tree 
--         searchTrees min max 
--             | min < max = oneof [ leafGen
--                     ,branchGen
--                 ]
--             | otherwise = leafGen
--                 where 
--                     leafGen:: Gen Tree 
--                     leafGen  = pure Leaf 
--                     branchGen :: Gen Tree 
--                     branchGen = choose (min, max)
--                                     >>= (\n -> Branch n <$>
--                                         searchTrees min (n -  1)  <*>
--                                         searchTrees n max
--                                         )