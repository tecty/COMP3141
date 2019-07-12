
class Functor f => App f where 
    pure :: a -> f a 
    tuple :: f a -> f b -> f (a, b)

    
(<**>) :: App f => f (a -> b) -> f a -> f b 
-- (tuple fab fa) will have type == f ((a->b), a)
-- tuple f a:: f (a->b) -> f a -> f b -> f ((a->b), a)
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Orinignal
-- fmap _(the function) (tuple f a) :: (a-> b) -> f a -> f b
-- Mapped
-- fmap  :: (((a->b), a)-> b) -> f ((a->b), a) -> f b
-- (<**>) fab fa = fmap (\(ffab, a) -> ffab a) (tuple fab fa)
(<**>) ffab fa = fmap (\(fab, a)-> fab a) (tuple ffab fa)



tuple' :: Applicative f => f a -> f b -> f (a ,b)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- fmap :: Functor f => (a -> b) -> f a -> f b
tuple' a b =  (,) <$> a <*> b


class Applicative m => Mon m where
    join :: m (m a) -> m a

-- fmap :: Functor f => (a -> b) -> f a -> f b

(>>>=) :: Mon m => m a -> (a -> m b) -> m b
(>>>=) a f = join $ fmap f a 
-- fmap f a :: (a -> m b) -> m a -> m (m b) 
-- fmap :: Functor f => (a -> b) -> f a -> f b

join' :: Monad m => m (m a) -> m a 
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
join' a = a >>= id