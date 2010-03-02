import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.State

-- 構文解析器結合子 ----------------------------------------------------

failure :: Parser t a
failure = mzero

succeed :: a -> Parser t a
succeed = return

item :: Parser t t
item = do { (x:xs) <- get
          ; put xs
          ; return x
          }

sat :: (t -> Bool) -> Parser t t
sat p = do { x <- item
           ; if p x then return x else failure
           }

alt :: Parser t a -> Parser t a -> Parser t a
alt = mplus

many :: Parser t a -> Parser t [a]
many p = many1 p `alt` return []

many1 :: Parser t a -> Parser t [a]
many1 p = do { x  <- p
            ; xs <- many p
            ; return (x:xs)
            }

-- 計算木の再構成 ------------------------------------------------------

data Term = Val Char | App Char Term Term

instance Show Term where
  show (Val c)     = [c]
  show (App o l r) = "(" ++ show l ++ [o] ++ show r ++ ")"


pterm, papp, pval :: Parser Char Term
pterm = papp `alt` pval

papp = do { sat ('('==)
          ; l <- pterm
          ; o <- pbop
          ; r <- pterm
          ; sat (')'==)
          ; return (App o l r)
          }

pval = do { c <- sat (`elem` "0123456789")
          ; return (Val c)
          }

pbop :: Parser Char Char
pbop = do { o <- sat (`elem` "+-*/")
          ; return o 
          }

type Rat = (Int,Int)

eval :: Term -> Rat
eval (Val x)     = ctor x
eval (App o l r) = (ctoo o) (eval l) (eval r)

ctor :: Char -> Rat
ctor x = (ord x - ord '0',1)

ctoo :: Char -> (Rat -> Rat -> Rat)
ctoo '+' (x,y) (z,w) = (x*w+z*y,y*w)
ctoo '-' (x,y) (z,w) = (x*w-z*y,y*w)
ctoo '*' (x,y) (z,w) = (x*z,y*w)
ctoo '/' (x,y) (z,w) = if z == 0 then (0,0) else (x*w,y*z)

-- List 版パーザと Maybe 版パーザ

type Parser t a = StateT [t] [] a        -- List 版
instance Read Term where
  readsPrec _ = runParser pterm

{-
type Parser t a = StateT [t] Maybe a     -- Maybe 版
instance Read Term where
  readsPrec _ = maybeToList . runParser pterm
-}

runParser = runStateT

-- List版とMaybe版の違いを見るための例 ---------------------------------

char :: Char -> Parser Char Char
char c = sat (c ==)

pS = do { a   <- pa
        ; bc  <- pB
        ; d   <- pd
        ; return ([a]++bc++[d])
        }

pB =       (do { b <- pb; return [b] })
     `alt` (do { b <- pb; c <- pc; return ([b]++[c]) })

pa = char 'a'
pb = char 'b'
pc = char 'c'
pd = char 'd'
