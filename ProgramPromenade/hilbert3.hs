import Control.Monad.State

{- 
状態モナドを使う.状態は亀の向いている方向
-}

type Order = Int
type Direction = (Int, Int)

data L = P | M | F deriving Show -- P が + に M が - にそれぞれ対応
type LSystem = [L]

p = (P:)
m = (M:)
f = (F:)

x, y :: Order -> (LSystem -> LSystem)
x 0     = id
x (n+1) = p  . y n .  f  .  m  . x n .  f  . x n .  m  .  f  . y n .  p
--        +     Y     F     -     X     F     X     -     F     Y     + 
y 0     = id
y (n+1) = m  . x n .  f  .  p  . y n .  f  . y n .  p  .  f  . x n .  m
--        -     X     F     +     Y     F     Y     +     F     X     -

{- 
*Main> x 0 []
[]
*Main> x 1 []
[P,F,M,F,M,F,P]
*Main> x 2 []
[P,M,F,P,F,P,F,M,F,M,P,F,M,F,M,F,P,F,P,F,M,F,M,F,P,M,F,M,F,P,F,P,F,M,P]
-}

hilbert :: Order -> LSystem
hilbert n = x n []

translate :: LSystem -> State Direction ShowS

translateP,translateM,translateF :: State Direction ShowS 
                                 -> State Direction ShowS
translateP sm = modify left  >> sm
translateM sm = modify right >> sm
translateF sm = get >>= \ d -> sm >>= \ ss -> return (fc d . ss)
  where fc (dx,dy) = shows dx.space.shows dy.space.showString "rlineto".newline

left, right :: Direction -> Direction
left  (dx,dy) = (-dy,dx)
right (dx,dy) = (dy,-dx)

space, newline :: ShowS
space   = showString " "
newline = showString "\n"

translate []     = return id
translate (P:ls) = translateP (translate ls)
translate (M:ls) = translateM (translate ls)
translate (F:ls) = translateF (translate ls)

moveto :: Int -> Int -> ShowS
moveto x y
 = shows x . space . shows y . space . showString "moveto" . newline

stroke :: ShowS
stroke = showString "stroke" . newline

drawHilbert :: Int -> Int -> ShowS
drawHilbert sz n
 = moveto o o
 . evalState (translate $ hilbert n) d0
 . stroke
  where d0      = (sz `div` 2 ^ n, 0)
        o       = sz `div` 2 ^ (n+1)

main = putStrLn $ drawHilbert 256 5 ""
