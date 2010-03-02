{- ストリームを使った対話型 Moo プログラム(山下) -}

{-
  dialogMoo: コンピュータの出題を対話しながらコンピュータが当てる
  playMoo:   コンピュータの出題を対話しながらひとが当てる
  helpMoo:   ひとの出題を対話しながらコンピュータが当てる
-}

module MooDialogue where
import Monad (foldM)
import Random

-- 問題の生成，候補のしぼり込みに使う順列の生成 ------------------------

perm :: [a] -> Int -> [[a]]          -- 順列の生成
perm _  0 = [[]]
perm [] _ = []
perm xs n = concat [ pm n hs ts | (hs,ts) <- splits xs ]
 where pm _ _  []     = []
       pm n hs (t:ts) = [ t:ys | ys <- perm (hs++ts) (n-1) ]

splits :: [a] -> [([a],[a])]
splits []         = [([],[])]
splits xxs@(x:xs) = ([],xxs) : [ (x:ys,zs) | (ys,zs) <- splits xs ]


nPr :: Int -> Int -> Int             -- 順列の数
nPr n r = product [n-r+1..n]

nthPerm :: [a] -> Int -> Int -> [a]  -- n番目の順列
nthPerm xs r m = nprm xs (length xs) r m
  where 
    nprm _  _ 0 0 = []
    nprm xs n r m = case divMod m (nPr (n-1) (r-1)) of
                      (p,q) -> case splitAt p xs of
                                 (xs,y:ys) -> y : nprm (xs++ys) (n-1) (r-1) q

---- クライアント/サーバ シミュレーション ---------------------------------

server :: (a -> b) -> ([a] -> [b])
server _       []         = []
server process (req:reqs) = process req : server process reqs 

client :: a -> (b -> a) -> ([b] -> [a])
client req0 next ~(resp:resps)
 = req0 : client (next resp) next resps

---- MOO ---------------------------------------------------------------

type Moo   = String
type Bulls = Int
type Cows  = Int
type Prod  = (Bulls,Cows)

type History    = [(Moo,Prod)]
type Candidates = [Moo]
type State      = (Candidates,Moo,History)

score :: Moo -> Moo -> Prod            -- Moo積を求める
score answer guess
 = (bulls,cows)
   where
     bulls = sum (zipWith ((fromEnum .) . (==)) answer guess)
     cows  = sum (map (fromEnum . (`elem` answer)) guess) - bulls
 
guess :: Prod -> State -> (Moo, State) -- Moo を推測する
guess prod (cads,moo,his)
 = (moo',(cads',moo',his'))
   where 
     his'       = (moo,prod):his
     moo':cads' = filter (\ x -> and [ score x m == p | (m,p) <- his' ]) cads

ndig :: Int
ndig = 4
digs ::  String
digs = "0123456789"
cs :: [Moo]
cs = perm digs 4

---- 質問者コンピュータ，解答者コンピュータ ----------------------------

dialogMoo
 = do { gen <- newStdGen
      ; let (r,_) = randomR (0,nPr (length digs) ndig - 1) gen
            ans   = nthPerm digs ndig r
            reqs  = client ("0123",(cs,"0123",[])) (uncurry guess) resps
            resps = server (fstApp (score ans)) reqs
            comms = map (uncurry (display ans)) 
                  $ zip (map fst reqs)
                        (takeWhile (/=(4,0)) (map fst resps)++[(4,0)])
        in mapM_ putStrLn comms
           >> putStrLn ("Computer got it in "++show (length comms)++" guesses.")
      }

fstApp f (x,y) = (f x,y)

display :: Moo -> Moo -> Prod -> String
display ans ""  _    = "Answer: "++ans
display ans moo prod 
 = "Guess : "++moo++"\nScore : "++show prod

---- 質問者コンピュータ，解答者ひと ------------------------------------

playMoo = do { gen <- newStdGen
             ; let (r,_) = randomR (0,nPr (length digs) ndig - 1) gen
                   ans   = nthPerm digs ndig r
                   reqs  = client getLine (const getLine) resps
                   resps = server (scoreIO ans) reqs
               in catch (foldM trav 0 resps) (const (return 0)) >> return ()
             }

scoreIO :: Moo -> IO Moo -> IO Prod
scoreIO ans moo = do { putStr "Guess: "; moo >>= return . score ans }

trav :: Int -> IO Prod -> IO Int
trav i iop = do { p <- iop
                 ; if p == (4,0)
                   then putStrLn ("You got it in "++show (i+1)++" guesses.")
                        >> fail ""
                   else (putStr "Score: " >> putStrLn (show p)) >> return (i+1)
                 }

---- 質問者ひと，解答者コンピュータ ------------------------------------

helpMoo 
 = let req0  = getLine >>= return . read
       reqs  = client req0 (const req0) resps
       resps = server id reqs
   in putStrLn "Guess: 0123" 
      >> catch (foldM guessIO (cs,"0123",[]) resps) (const (return ([],"",[])))
      >> return ()
                  
guessIO :: State -> IO Prod -> IO State
guessIO s iop = do { putStr "Score: "
                   ; p <- iop
                   ; let (m,s'@(cs,_,_)) = guess p s
                     in  putStrLn ("Guess: "++m) 
                         >> if null cs
                               then fail "You must get it."
                               else return s'
                   }
