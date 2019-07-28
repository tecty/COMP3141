s :: Monad m => [m a] -> m [a]
s [] = pure []
s (a:as) = do
  x <- a
  xs <- s as
  pure (x : xs)


m :: Monad m => (a -> m b) -> [a] -> m [b]
m = s . map