-- 各モジュールはファイル名を「モジュール名.hs」として保存
module Mark ( Mark(..), isEmpty, next ) where
	   -- Mark(..) データ型名とその構築子名をエクスポート
	   -- Mark     データ型名のみエクスポート

data Mark = O	-- ○，先手の印
	  | X	-- ×，後手の印
	  | U	-- 空き，何も置かれていない
	  | B	-- 障害物，何も置けない
            deriving Eq

instance Show Mark where
  show O = "O"
  show X = "X"
  show U = "."
  show B = "*"

isEmpty :: Mark -> Bool
isEmpty = (U ==)

next :: Mark -> Mark		-- 手番を一つ進める
next O = X
next X = O
