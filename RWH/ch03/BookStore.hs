-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                  deriving (Show)
data MagazineInfo = Magazine Int String [String]
                  deriving (Show)
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
myMag = Magazine 1000001 "Weekly Uekigo"
        ["Ueki Go"]
data BookReview = BookReview BookInfo CustomerID String
type CustomerID = Int
type ReviewBody = String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody
type ReviewRecord = (BookInfo, BookReview)
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivary
                 | Invoice CustomerID
                   deriving (Show)