module Ch11.Prettify2 where
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)
empty :: Doc
empty = Empty
(<>) :: Doc -> Doc -> Doc
a <> Empty = a
Empty <> b = b
a <> b = Concat a b