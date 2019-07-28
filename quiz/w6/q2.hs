data NonEmptyList a = One a | Cons a (NonEmptyList a)

instance Functor NonEmptyList where
    fmap f (One x) = One (f x)
    fmap f (Cons x xs) = Cons (f (f x)) (fmap f xs)