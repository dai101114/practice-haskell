-- 9. 関数altMap :: (a -> b) -> (a -> b) -> [a] -> [b] を定義してください。
-- - この関数は、引数で指定された二つの関数をリストの要素に交互に適用します。
-- 以下に使用例を示します。
-- > altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 xs = map ((\(x, i) -> ([f1, f2] !! (i `mod` 2)) $ x)) (zip xs [0 ..])

-- 別解
altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' f1 f2 = zipWith ($) (cycle [f1, f2])

altMap'' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap'' _ _ [] = []
altMap'' f1 _ [x] = [f1 x]
altMap'' f1 f2 (x1 : x2 : xs) = f1 x1 : f2 x2 : altMap'' f1 f2 xs

-- Luhn アルゴリズムは、銀行のカード番号に対して単純な入力間違いを検出する方法であり、以下のように実行されます。
-- - それぞれを独立した番号だとみなす
-- - 右から数えて偶数番めの数すべてを二倍にする
-- - それぞれの数が9 より大きいなら9 を引く
-- - すべての数を足し合わせる
-- - 合計が10で割り切れるなら、カードの番号は正しい

luhnDouble :: Int -> Int
luhnDouble n
  | n * 2 > 9 = n * 2 - 9
  | otherwise = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

-- 10. 第4章の練習問題に出てきたLuhnアルゴリズムを実装する関数を、任意の長さのカード番号を取り扱えるように改良してください。
-- - altMapを用いて関数luhn :: [Int] -> Bool を定義してください。
-- - 自分の銀行のカード番号を使ってテストしましょう。

luhn' :: [Int] -> Bool
luhn' ns = sum (altMap firstFn secondFn ns) `mod` 10 == 0
  where
    (firstFn, secondFn) = if odd $ length ns then (id, luhnDouble) else (luhnDouble, id)
