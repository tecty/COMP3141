data Tree a 
    = Leaf 
    | Node a (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where 
    fmap f Leaf = Leaf 
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative Tree where 
    -- pure a -> Tree a 
    pure x = Node x (pure x) (pure x)
    -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b 
    Node f fls frs <*> Node a als ars 
        = Node (f a) (fls <*> als) (frs <*> ars)
