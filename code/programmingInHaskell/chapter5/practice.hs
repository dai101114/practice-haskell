import Data.Char

-- 1. リスト内包表記を使って、1 から100 までの二乗の和1^2 + 2^2 + ... + 100^2 を計算する式を考えてください。
-- ans: sum [x^2 | x <- [1..100]]

-- 2. m×n の座標格子が、0 ≦ x ≦ m、0 ≦ y ≦ n に対し、すべての整数の組(x, y)で表現されているとします。
-- リスト内包表記を一つ用いて、与えられた大きさの座標格子を返す関数grid :: Int -> Int -> [(Int,Int)] を定義してください。
-- 以下に使用例を示します。
-- > grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- 3. リスト内容表記一つと上記の関数grid を用いて、大きさn の正方形座標を返す関数square :: Int -> [(Int,Int)] を定義してください。
-- ただし、(0, 0), (n, n)の対角の格子は含みません。以下に使用例を示します。
-- > square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4. ある要素のみからなるリストを生成するプレリュード関数replicate :: Int -> a -> [a] を, 関数length と同じ要領でリスト内包表記を用いて定義してください。
-- 以下に使用例を示します。
-- > replicate 3 True
-- [True,True,True]
replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1 .. n]]

-- 5. x^2 + y^2 = z2 を満たす正の整数をピタゴラス数と呼び、三つ組(x, y, z) で表します。
-- ピタゴラス数のリストを生成する関数pyths :: Int -> [(Int, Int, Int)] をリスト内包表記を使って定義してください。
-- ただし、ピタゴラス数の要素は与えられた上限以下であるとします。以下に例を示します。
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

-- 6. 自分自身を除く約数の和が自分自身と等しいとき、その整数を完全数と呼びます。
-- 与えられた上限までに含まれる完全数すべてを算出する関数perfects:: Int -> [Int] を、リスト内包表記と関数factors を使って定義してください。
-- 以下に使用例を示します。
-- > perfects 500
-- [6,28,496]
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], (sum $ factors x) == 2 * x]

-- 7. 二つの生成器を持つリスト内包表記[(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]] は
-- 一つの生成器を持つリスト内包表記二つでも表現できることを示してください。
-- ヒント：一方のリスト内包表記を他方の中に入れ、プレリュード関数concat を使いましょう。
-- ans: concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]

-- 8. 関数positions を関数find を使って再定義してください。
find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- original positions
-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..], x == x' ]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x $ zip xs [0 ..]

-- 9. 長さがn である整数のリストxs とys の内積は、対応する要素の積の和として計算できます。
-- 二つのリストから内積を計算する関数scalarproduct :: [Int] -> [Int] -> Int を
-- 関数chisqr と同じようにリスト内包表記を使って定義できることを示してください。以下に使用例を示します。
-- > scalarproduct [1,2,3] [4,5,6]
-- 32

-- chisqr 実装
-- chisqr :: [Float] -> [Float] -> Float
-- chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 10. シーザー暗号のプログラムを変更して大文字も扱えるようにしてください。

-- 元のプログラム
-- シーザー暗号暗号化関数
-- encode :: Int -> String -> String
-- encode n xs = [shift n x | x <- xs]

-- shift :: Int -> Char -> Char
-- shift n c | isLower c = int2let ((let2int c + n)` mod` 26)
--           | otherwise = c

-- int2let :: Int -> Char
-- int2let n = chr (ord 'a' + n)

-- let2int :: Char -> Int
-- let2int c = ord c - ord 'a'

-- -- シーザー暗号解読関数
-- crack :: String -> String
-- crack xs = encode (-factor) xs
--           where
--             factor = head (positions (minimum chitab) chitab)
--             chitab = [chisqr (rotate n table' ) table | n <- [0..25]]
--             table' = freqs xs

-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- chisqr :: [Float] -> [Float] -> Float
-- chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- rotate :: Int -> [a] -> [a]
-- rotate n xs = drop n xs ++ take n xs

-- table :: [Float]
-- table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
--           0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
--           6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- freqs :: String -> [Float]
-- freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
--           where n = lowers xs

-- percent :: Int -> Int -> Float
-- percent n m = (fromIntegral n / fromIntegral m) * 100

-- count :: Char -> String -> Int
-- count x xs = length [x' | x' <- xs, x == x']

-- lowers :: String -> Int
-- lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- 修正後のプログラム
-- シーザー暗号暗号化関数
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2lower ((lower2int c + n) `mod` 26)
  | isUpper c = int2Upper ((upper2int c + n) `mod` 26)
  | otherwise = c

int2lower :: Int -> Char
int2lower n = chr (ord 'a' + n)

lower2int :: Char -> Int
lower2int c = ord c - ord 'a'

int2Upper :: Int -> Char
int2Upper n = chr (ord 'A' + n)

upper2int :: Char -> Int
upper2int c = ord c - ord 'A'

-- シーザー暗号解読関数
crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Float]
table =
  [ 8.1,
    1.5,
    2.8,
    4.2,
    12.7,
    2.2,
    2.0,
    6.1,
    7.0,
    0.2,
    0.8,
    4.0,
    2.4,
    6.7,
    7.5,
    1.9,
    0.1,
    6.0,
    6.3,
    9.0,
    2.8,
    1.0,
    2.4,
    0.2,
    2.0,
    0.1
  ]

freqs :: String -> [Float]
freqs xs = [percent ((count x xs) + (count (toUpper x) xs)) n | x <- ['a' .. 'z']]
  where
    n = chars xs

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

chars :: String -> Int
chars xs = length [x | x <- xs, (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')]
