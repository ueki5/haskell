a = ("Porpoise", "Grey")
b = ("Table", "Oak")
data Cetacean = Cetacean String String 
  deriving (Show)
data Furniture = Furniture String String
  deriving (Show)
c = Cetacean "Porpoise" "Grey"
d = Furniture "Table" "Oak"
c' = Cetacean "Porpoise!" "Grey?"
ax = [a,b]
ax' = [c,c']
