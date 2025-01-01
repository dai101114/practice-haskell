-- 1. 再帰的に定義された階乗関数は、(-1) のように負の整数を与えられた場合、どのように振る舞うでしょうか？
-- 再帰部にガードを加えることで、負の整数を禁止するように定義を変更してください。

-- ans: ガードが無い場合、基底部に到達しないため無限ループとなる。

factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0     = n * factorial (n - 1)

-- 2. 与えられた非負の整数から0 までを足し合わせる関数sumdown :: Int -> Int を再帰的に定義してください。
-- たとえば、sumdown 3 は3+2+1+0 の結果6 を返します。

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n - 1)
          | otherwise = 0

-- 3. 乗算演算子* の再帰を参考にして、負でない整数に対する冪乗演算子^ を定義してください。
-- また、その定義を使って2 ^ 3 を簡約してください。

(^) :: (Num a, Integral b) => a -> b -> a
_ ^ 0 = 1
n ^ m = n * (n Main.^ (m - 1))

-- 簡約
-- 2 ^ 3 = 2 * (2 ^ 2)
--       = 2 * 2 * (2 ^ 1)
--       = 2 * 2 * 2 * (2 ^ 0)
--       = 2 * 2 * 2 * 1
--       = 8

-- 4. 二つの非負の整数に対する最大公約数を計算するために、ユークリッドの互除法を実現する関数euclid :: Int -> Int -> Int を再帰的に定義してください。
-- アルゴリズムでは以下の工程を繰り返します。すなわち、二つの数値が等しければ、それが答えです。
-- そうでなければ、「小さいほう」と「大きいほうから小さいほうを引いた数」で互除法を繰り返します。以下に使用例を示します。
-- > euclid 6 27
-- 3

euclid :: Int -> Int -> Int
euclid n m | n == m     = n
           | n < m      = euclid (m-n) n
           | otherwise  = euclid m (n-m)

-- 5. この章で与えられた再帰的定義を使って、length [1,2,3]、drop 3 [1,2,3,4,5]、init [1,2,3] を簡約してください。

-- length :: [a] -> Int
-- length [] = 0
-- length (_:xs) = 1 + length xs

-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs
-- drop _ [] = []
-- drop n (_:xs) = drop (n-1) xs

-- init :: [a] -> [a]
-- init [_] = []
-- init (x:xs) = x : init xs

-- ans:
-- length [1,2,3] = 1 + length [2,3]
--                = 1 + 1 + length [3]
--                = 1 + 1 + 1 + length []
--                = 1 + 1 + 1 + 0
--                = 3
-- drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5]
--                    = drop 1 [3,4,5]
--                    = drop 0 [4,5]
--                    = [4,5]
-- init [1,2,3] = 1 : init [2,3]
--              = 1 : (2 : init [3])
--              = 1 : (2 : [])
--              = [1,2]

-- 6. プレリュードを見ないで、リストに対する以下のプレリュード関数を再帰を使って定義してください。
-- a リストの要素がすべてTrue であるか検査する関数
and :: [Bool] -> Bool
and [] = True
and (b:bs) = b && Main.and bs
-- b リストのリストを取り、要素であるリストを連結する関数
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ Main.concat xss
-- c 指定された要素をn 個持つリストを生成する関数
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : Main.replicate (n-1) x
-- d 空でないリストのn 番めの要素を取り出す関数
(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs Main.!! (n - 1)
-- e リストの要素に含まれるか検査する関数
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) = a == x || Main.elem a xs

-- これらの関数の多くは、プレリュードでは再帰ではなく他のプレリュード関数を用いて定義されています。
-- また、リスト型に特化せず、より汎用的な関数になっています。

-- 7. 関数merge :: Ord a => [a] -> [a] -> [a] は、整列されたリストを二つ取り、一つの整列されたリストにして返す関数です。
-- 以下に使用例を示します。
-- > merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
-- 関数merge を再帰を用いて定義してください。ただし、関数insert やisortなど、整列されたリストを処理する関数は利用してはいけません。

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8. 関数merge を使って、マージソートを実行する関数msort :: Ord a => [a] -> [a] を定義してください。
-- マージソートは、引数のリストを二つに分割し、それぞれを整列した後で再び一つに戻すことで整列を実現します。
-- ただし、空リストと要素が一つのリストはすでに整列されていると考えます。
-- ヒント：最初に、リストを半分に分割する関数halve :: [a] -> ([a], [a]) を定義してください。生成された二つのリストの長さは高々1 しか違いません。

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs') (msort ys')
  where
    (xs', ys') = halve xs

-- 9. 五段階の工程を使って、以下のプレリュード関数を定義してください。
-- a 数値のリストに対し要素の和を計算する関数sum
-- b リストの先頭からn 個の要素を取り出す関数take
-- c 空でないリストの末尾の要素を取り出す関数last

-- 第一段階：型を定義する
-- 第二段階：場合分けをする
-- 第三段階：場合分けの簡単なほうを定義する
-- 第四段階：場合分けの複雑なほうを定義する
-- 第五段階：一般化し単純にする

-- a
-- 1
-- sum :: Num a => [a] -> a
-- 2
-- sum [] = 
-- sum (x:xs) = 
-- 3
-- sum [] = 0
-- 4
-- sum (x:xs) = x + sum xs

-- b
-- 1
-- take :: Int -> [a] -> [a]
-- 2
-- take 0 [] = 
-- take 0 (x:xs) = 
-- take n [] = 
-- take n (x:xs) = 
-- 3
-- take 0 [] = []
-- take 0 (x:xs) = []
-- take n [] = []
-- 4
-- take n (x:xs) = x : take (n-1) xs

-- c
-- 1
-- last :: [a] -> a
-- 2
-- last [x]
-- last (x:xs) = 
-- 3
-- last [x] = x
-- 4
-- last (x:xs) = last xs

