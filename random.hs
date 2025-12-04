import Prelude hiding (Monoid, Semigroup)

-- data Maybe a = Nothing | Just a deriving Show

-- instance Functor Maybe where
--     fmap f (Just a) = Just (f a)
--     fmap _ _ = Nothing 

-- instance Functor ((,), x) where
--     fmap :: (a -> b) -> (x, a) -> (x ,b)
--     fmap f (x, a) = (x, f a)
--     fmap f Nothing

-- data Treee a = Leaf a | Sprout a a | Fork (Treee a) a (Treee a)

-- instance Functor Tree where
--     fmap :: (a -> b) ->  Treee a -> Treee b
--     fmap f (Leaf a) = Leaf ( f a)


class Semigroup s where
    (<>) :: s -> s -> s
class Semigroup m => Monoid m where
    mempty :: m
instance Monoid m => Applicative ((,) m) where
    pure :: a -> (m, a)
    pure (x, f) = (x, id)
    (<*>) :: (m, a -> b) -> (m, a) -> (m, b)
    (x, f) <*> (y, a) = (x <*> y, f a)