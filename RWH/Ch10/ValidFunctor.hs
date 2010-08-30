module ValidFunctor where
data Foo a = Foo a
instance Functor Foo where
    fmap f (Foo a) = Foo (f a)
-- invalid functor !!!
-- data Bar a = Bar a
-- data (Eq a) => Bar a = Bar a
-- -- -- instance Functor Bar where
-- -- --     fmap f (Bar a) = Bar (f a)
-- -- fmap :: (Eq a) => (a -> b) -> Foo a -> Foo b
-- -- instance Functor Bar where
-- --     fmap f (Bar a) = Bar (f a)
-- -- myMap :: (Eq a,b) => (a -> b) -> Bar a -> Bar b
-- myMap :: (Eq a,Eq b) => (a -> b) -> Bar a -> Bar b
-- myMap f (Bar a) = Bar (f a)
-- -- instance Functor Bar where
-- --     fmap = myMap
-- --     fmap :: (Eq a,Eq b) => (a -> b) -> Bar a -> Bar b
-- --     fmap f (Bar a) = Bar (f a)
