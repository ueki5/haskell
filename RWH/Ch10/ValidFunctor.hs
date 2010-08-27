module ValidFunctor where
data Foo a = Foo a
instance Functor Foo where
    fmap f (Foo a) = Foo (f a)
data Eq a => Bar a = Bar a
instance Functor Bar where
--     fmap :: (Eq a,b) => (a -> b) -> f a -> f b
    fmap f (Bar a) = Bar (f a)