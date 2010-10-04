module Ch12.BitList where
data Bit = Zero
         | One
         deriving (Show)
type BitList = [Bit]
makeBitList :: Int -> [BitList]
makeBitList 0 = [[]]
makeBitList (n + 1) = (map (Zero:) (makeBitList n)) ++ (map (One:) (makeBitList n))
oddList :: BitList -> Bool
oddList bs = odd . length $ filter zero bs
evenList :: BitList -> Bool
evenList bs = even . length $ filter zero bs
zero Zero = False
zero One = True
