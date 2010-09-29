module VCard where
import Control.Monad

data Context = Home | Mobile | Business
               deriving (Eq, Show)
type Phone = String
albulena = [(Home, "+355-652-55512")]
nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]
twalumba = [(Business, "+260-02-55-5121")]
uekigo = [(Mobile, "090-8741-5052")]

oneParsonalPhone :: [(Context, Phone)] -> Maybe Phone
oneParsonalPhone [] = Nothing
-- oneParsonalPhone (x:xs) = if match x
--                           then Just (snd x)
--                           else oneParsonalPhone xs
--     where match :: (Context, Phone) -> Bool
--           match (Home, _) = True
--           match (Mobile, _) = True
--           match _ = False
oneParsonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps 
                        Just phone -> Just phone

allBusinessPhones :: [(Context, Phone)] -> [Phone]
-- allBusinessPhones ps = do
--     (ctx, phone) <- ps
--     if ctx == Business then [phone] else []
-- allBusinessPhones ps = let phones = getBusiness Business ps
--                            in if phones == [] 
--                               then getBusiness Mobile ps
--                               else phones
--     where getBusiness :: Context -> [(Context, Phone)] -> [Phone]
--           getBusiness c [] = []
--           getBusiness c (p:ps) = if c == fst p 
--                                  then [snd p] ++ (getBusiness c ps)
--                                  else getBusiness c ps
allBusinessPhones ps = map snd pairs
    where pairs = case filter (isContext Business) ps of
                    [] -> filter (isContext Mobile) ps
                    pairs' -> pairs'
isContext :: Context -> (Context, Phone) -> Bool
isContext c (c', phones) = c == c'
oneBusinessPhones ps = lookup Business ps `mplus` lookup Mobile ps
allParsonalPhones ps = map snd $ filter (isContext Home) ps `mplus` 
                                 filter (isContext Mobile) ps
lookupM :: (MonadPlus m,Eq a) => a -> [(a, b)] -> m b
lookupM _ [] = mzero
lookupM key (p, ps) = if p == key
                      then 