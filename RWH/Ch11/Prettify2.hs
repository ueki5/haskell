module Ch11.Prettify2 where
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)
-- empty :: Doc
-- empty = undefined
-- (<>) :: Doc -> Doc -> Doc
-- (<>) = undefined