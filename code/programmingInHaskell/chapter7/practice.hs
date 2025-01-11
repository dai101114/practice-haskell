-- 1. リスト内包表記[f x | x <- xs, p x] は、高階関数map とfilter を使ってどう書き直せるでしょうか。
myListComprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
myListComprehension f p xs = (map f . filter p) xs

-- 2. プレリュードでの定義を見ないで以下の高階関数を定義してください。
-- a. リストの要素のすべてが述語を満たすか検査する関数
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = length (filter p xs) == length xs

-- b. リストの要素のどれかが述語を満たすか検査する関数
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = length (filter p xs) > 0

-- c. リストの先頭から述語を満たす連続した要素を取り出す関数
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- d. リストの先頭から述語を満たす連続した要素を取り除く関数
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = (x : xs)

-- 3. 関数foldr を用いて、関数map f とfilter p を定義してください。
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- 4. foldl を用いて、十進表記を整数に変換する関数
-- 以下に使用例を示します。
-- > dec2int [2,3,4,5]
-- 2345
dec2int :: [Int] -> Int
dec2int [] = 0
dec2int ns = foldl (\acc (n, ri) -> acc + n * 10 ^ ri) 0 $ zip ns (reverse [0 .. length ns - 1])

-- 別解
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> 10 * acc + x) 0

-- 5. プレリュードの定義を見ないで以下の二つの高階関数を定義してください。
-- a. 「引数に組を取る関数」を「カリー化された関数」へ変換する関数
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

-- b. 「引数が二つのカリー化された関数」を「引数に組を取る関数」へ変換する関数
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- 6. unfold は、リストを生成する単純な再帰の様式を閉じ込めた高階関数で、以下のように定義できます。
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- すなわち、関数unfold p h t は、述語p を引数に適用した結果が真となれば空リストを返します。そうでなければ、関数h を引数へ適用することで先頭の
-- 要素を作り、残りのリストは自分自身を呼び出すことで生成して、全体として空でないリストを作ります。再帰する際には、関数t を引数に適用して、新たな引数を作ります。
-- たとえば、関数unfold を使って関数int2bin をもっと簡潔に書き直せます。
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- 関数unfold を用いて関数chop8、map f、iterate f を再定義してください。

-- chop8 サンプル
chop8 :: [Int] -> [[Int]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' :: [Int] -> [[Int]]
chop8' = unfold (== []) (take 8) (drop 8)

map'' :: (Eq a) => (a -> b) -> [a] -> [b]
map'' f = unfold (== []) (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f
