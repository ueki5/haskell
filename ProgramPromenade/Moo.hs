-- 数当てゲーム MOO

import Random 
import List ((\\), nub)    -- Listのモジュールを取り込み (\\), nub を使う
import Char (isDigit)

type PackedInt = Int
type UnpackedInt = [Int]
type MOOProduct = (Int, Int)


-- ユーティリティ関数

showUnpackedInt :: UnpackedInt -> String
showUnpackedInt ds = concatMap show ds
    -- concatMap f = concat . map f

showMOOProduct :: MOOProduct -> String
showMOOProduct (bulls, cows) =
  "  Bulls: " ++ show bulls ++ ", Cows: " ++ show cows

pack :: UnpackedInt -> PackedInt
pack ds = foldl (\ x y -> 10*x + y) 0 ds

unpack :: Int -> PackedInt -> UnpackedInt
unpack 1 d = [d]
unpack (n+1) d = unpack n q ++ [r]
  where
    (q, r) = d `divMod` 10

disjoint :: UnpackedInt -> Bool
disjoint xs = length xs == length (nub xs)

mooNum :: Int -> MOOProduct -> Bool
mooNum n (bulls, cows) = bulls == n && cows == 0


-- 出題プログラム

main :: IO ()
main = moo 4		-- コンパイルして生成されるのは4桁版

moo :: Int -> IO ()
moo n = do
  randomGen <- newStdGen
  let randoms = randomRs (0, 10^n-1) randomGen
	-- 0 <= r < 10^n を満たす乱数 r の無限列を生成 
      answer = (head . filter disjoint . map (unpack n)) randoms
	-- n 桁の数に重複のないものを一つ抽出
  loop n answer []

loop :: Int -> UnpackedInt -> [UnpackedInt] -> IO ()
loop n answer history = do
  putStr "Your guess? "			-- 入力を促す
  guessStr <- getLine			-- 改行までの文字列を入力
  if not (legal guessStr)
    then loop n answer history		-- redo
    else let guess = unpack n (read guessStr :: Int)
	     mooProduct = score answer guess in
         if mooNum n mooProduct
	   then do putStrLn ("  You got it in " ++
			     show (1+length history) ++ " guesses!")
	   else do putStrLn (showMOOProduct mooProduct)
		   loop n answer (guess:history)
  where
    legal str = length str == n && and (map isDigit str)

score :: UnpackedInt -> UnpackedInt -> MOOProduct
score xs ys = (bulls, cows)
  where
    (xs', ys') = unzip [ (x,y) | (x,y) <- zip xs ys, x /= y ]
    bulls = length xs - length xs'
    cows = length xs' - length (xs' \\ ys')


-- 解答プログラム

digits :: [Int]
digits = [0..9]

gen :: Int -> [UnpackedInt]
gen n = gens n []
  where
    gens :: Int -> [Int] -> [UnpackedInt]
    gens 0 rs = if disjoint rs then [rs] else []
    gens (n+1) rs = concatMap (\r -> gens n (rs++[r])) digits

{- 参考: gen4 == gen 4
gen4 = [ ds | p<-digits, q<-digits, r<-digits, s<-digits,
             let ds=[p,q,r,s], disjoint ds]
-}

solver :: Int -> Int -> IO ()
solver n answer = putStr result 
  where
    score1 = score (unpack n answer)		-- 予想からMOO積を返す関数
    candidates = gen n
    result = solver1 n score1 [] candidates

solver1 :: Int -> (UnpackedInt -> MOOProduct) ->
           [UnpackedInt] -> [UnpackedInt] -> String
solver1 n score1 history (guess:rs) = solver2 n score1 history rs guess

solver2 :: Int -> (UnpackedInt -> MOOProduct) ->
	   [UnpackedInt] -> [UnpackedInt] -> UnpackedInt -> String
solver2 n score1 history rs guess =
  let mooProduct = score1 guess in
  if mooNum n mooProduct
    then "Computer found " ++ showUnpackedInt guess
	 ++ " in " ++ show (1+length history) ++ " guesses."
    else let rs' = [ r | r <- rs, score guess r == mooProduct ] in
	 "Computer guesses " ++ showUnpackedInt guess ++ "\n"
	 ++ showMOOProduct mooProduct ++ "\n"
	 ++ solver1 n score1 (guess:history) rs'
