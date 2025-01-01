-- n = a `div` length xs
--     where
--         a = 10
--         xs = [1,2,3,4,5]

-- last xs = head $ reverse xs
-- init xs = take $ length xs - 1 xs
-- init xs = reverse $ drop 1 $ reverse xs

-- 1
['a','b','c'] :: String
('a','b','c') :: (Char, Char, Char)
[(False,'0'),(True,'1')] :: [(Bool, Char)]
([False,True],['0','1']) :: ([Bool], [Char])
[tail, init, reverse] :: [[a] -> [a]]

-- 2 以下の型を持つ定義を書き下してください。型が正しい限り、どのように実装してもかまいません。
bools :: [Bool]
bools = [True, False]
nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]
add :: Int -> Int -> Int -> Int
add a b c = a + b + c
copy :: a -> (a,a)
copy a = (a, a)
apply :: (a -> b) -> a -> b
apply f a = f a

-- 3 以下の関数の型は何でしょう？
second :: [a] -> a
second xs = head (tail xs)
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
pair :: a -> b -> (a,b)
pair x y = (x,y)
double :: Num a => a -> a
double x = x*2
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs
twice :: (a -> a) -> a -> a
twice f x = f (f x)
