data (Ord a) => OrdStack a = Bottom
                           | Item a (OrdStack a)
                             deriving (Show)
isIncreasing :: (Ord a) => OrdStack a -> Bool
-- isIncreasing (Item e stck@(Item e' _)) = case e < e' of
--                                          True -> isIncreasing stck
--                                          False -> False
isIncreasing (Item e stck@(Item e' _)) 
             | e < e' = isIncreasing stck
             | otherwise = False
-- isIncreasing Bottom = True
-- isIncreasing (Item e Bottom) = True
isIncreasing _ = True
push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a stck = Item a stck
pop :: (Ord a) => OrdStack a -> (Maybe a, OrdStack a)
pop Bottom = (Nothing, Bottom)
pop (Item a stck) = (Just a, stck)

