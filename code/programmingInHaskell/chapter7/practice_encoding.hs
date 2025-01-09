import Data.Char

type Bit = Int

-- 符号化関数
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n` mod` 2 : int2bin (n` div` 2)

-- 復号化関数
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- 通信
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7. パリティービットの概念を用いて、文字列の二進数への変換器が単純な通信エラーを検出できるように改良してください。
-- - 符号化で生成された8ビットの二進数に、1の数が奇数なら1、そうでないなら0になるパリティービットを付加します。
-- - 復号の際は9ビットの二進数のパリティービットが正しいかを検査し、正しければパリティービットを捨て、誤りであればエラーを報告します。
-- ヒント：評価を強制終了し、与えられた文字列をエラーメッセージとして表示するには
-- プレリュード関数error :: String -> a を使います。error は返り値の型が多相的なのでどこでも利用できます。

-- 8. 通信エラーの生じる通信路を用いて、直前の問題で定義した文字列を通信するプログラムを試してください。
-- - この通信路は最初のビットを落とすものとします（関数tailをビットのリストに適用することで実現できます）。
