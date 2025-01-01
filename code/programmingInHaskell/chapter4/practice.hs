-- 1. プレリュード関数を使って、長さが偶数のリストを半分ずつに分割する関数
-- halve :: [a] -> ([a], [a]) を定義してください。以下に使用例を示し
-- ます。
-- > halve [1,2,3,4,5,6]
-- ([1,2,3],[4,5,6])
halve :: [a] -> ([a], [a])
halve ns = (take len ns, drop len ns)
           where
             len = (length ns) `div` 2

-- 2. リストの三つめの要素を返す関数third :: [a] -> a を、以下を使ってそれぞれ定義してください。ただし、リストには三つ以上の要素が格納されているとします。
-- a head とtail
-- b リストのインデックス演算子!!
-- c パターンマッチ

third :: [a] -> a
-- a
third xs = head $ tail $ tail xs
-- b
third xs = xs !! 2
-- c
third [_,_,x] = x
-- third (_:_:x:_) = x

-- 3. プレリュード関数tail のように振る舞うsafetail :: [a] -> [a] 関数を考えてください。
-- ただし、tail は空リストを与えるとエラーになりますが、safetail は空リストをエラーとせず、空リストを返すものとします。
-- 関数tail、空リストかどうかを判定する関数null :: [a] -> Bool、および以下のそれぞれを使ってsafetail を定義してください。
-- a 条件式
-- b ガード付きの等式
-- c パターンマッチ

safetail :: [a] -> [a]
-- a
safetail xs = if null xs then [] else tail xs
-- b
safetail xs | null xs = []
            | otherwise = tail xs
-- c
safetail [] = []
safetail xs = tail xs

-- 4. 論理積演算子&&と同様に、パターンマッチを使って論理和演算子|| を四通りの方法で定義してください。
(||) :: Bool -> Bool -> Bool

-- 4. 1通り目
True || True = True
True || False = True
False || True = True
False || False = False

-- 4. 2通り目
False || False = False
_ || _ = True

-- 4. 3通り目
True || _ = True
False || b = b

-- 4. 4通り目
a || b | a == b = a
       | otherwise = True

-- 5. 他のプレリュード関数や演算子を使わずに、論理積&& に対する以下の定義を条件式を用いて形式化してください。
-- True && True = True
-- _ && _ = False
-- ヒント：入れ子になった二つの条件式を使いましょう。

a && b = if a
            then if b then True else False
            else False

-- 6. 以下についても同様のことをしてください。必要になる条件式の個数が異なることに注意しましょう。
-- True && b = b
-- False && _ = False

a && b = if a then b else False

-- 7. 以下のカリー化された関数の定義の意味をラムダ式を用いて形式化してください。
-- mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z

mult x y z = (\x y z -> x*y*z) x y z
-- mult = \x -> \y -> \z -> x * y * z

-- 8. Luhn アルゴリズムは、銀行のカード番号に対して単純な入力間違いを検出する方法であり、以下のように実行されます。
-- - それぞれを独立した番号だとみなす
-- - 右から数えて偶数番めの数すべてを二倍にする
-- - それぞれの数が9 より大きいなら9 を引く
-- - すべての数を足し合わせる
-- - 合計が10で割り切れるなら、カードの番号は正しい

-- 数を2倍にして、もしその結果が9より大きいなら9を引く関数luhnDouble:: Int -> Int を定義してください。
-- 使用例を以下に示します。
-- > luhnDouble 3
-- 6
-- > luhnDouble 6
-- 3
-- luhnDouble と整数の剰余を求める関数mod を使って、4 桁の銀行のカード番号が正しいかどうかを判定する関数luhn :: Int -> Int -> Int -> Int -> Bool を定義してください。
-- > luhn 1 7 8 4
-- True
-- > luhn 4 7 8 3
-- False

luhnDouble :: Int -> Int  
luhnDouble n | n * 2 > 9 = n * 2 - 9
             | otherwise = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
