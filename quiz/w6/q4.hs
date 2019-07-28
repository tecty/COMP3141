data NonEmptyList a = One a | Cons a (NonEmptyList a)

instance Functor NonEmptyList where 
    fmap f (One x) = One $ f x 
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- instance Applicative NonEmptyList where
--     pure x = Cons x (pure x)
--     (One f) <*> (One x) = One (f x)
--     (One f) <*> (Cons x _) = One (f x)
--     (Cons f _) <*> (One x) = One (f x)
--     (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)
