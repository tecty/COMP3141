import Control.Monad (ap)

data Variable = A | B | C deriving (Show, Eq)

data Formula v = Var v 
    | Plus (Formula v) (Formula v)
    | Times (Formula v) (Formula v)
    | Constant Int 
    deriving (Eq, Show)
example :: Formula Variable 
example = Plus (Times (Var A) (Var A)) (Times (Var B) (Var C))


instance Functor Formula where 
    fmap f (Var v)         = Var (f v)
    fmap f (Constant i)    = Constant i
    fmap f (Times fol for) = Times (fmap f fol) (fmap f for)
    fmap f (Plus fol for)  = Plus (fmap f fol) (fmap f for)

instance Applicative Formula where 
    (<*>) = ap
        -- where 
        --     ap :: Formula (a -> b) -> Formula a -> Formula b 
        --     ap f a = 
        --         do  f' <- f 
        --             a' <- a 
        --             pure (f' a')
    pure = Var 


instance Monad Formula where 
    Var x >>= f      = f x 
    Constant i >>= f = Constant i 
    Plus  l r >>= f  = Plus  (l >>= f) (r >>= f)
    Times l r >>= f  = Times (l >>= f) (r >>= f)

subst A = Constant 3
subst B = Constant 6
subst C = Constant 1