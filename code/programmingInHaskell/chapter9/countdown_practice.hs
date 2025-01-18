{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0

-- valid Add x y = x <= y
-- valid Sub x y = x > y
-- valid Mul x y = x /= 1 && y /= 1 && x <= y
-- valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
  [ apply o x y
  | x <- eval l,
    y <- eval r,
    valid o x y
  ]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

-- 1. 関数合成 concat および map の代わりにリスト内包表記を使って、
--    組み合わせの関数 choices を再定義してください。

choices :: [a] -> [[a]]
choices = concatMap perms . subs

choices' :: [a] -> [[a]]
choices' xs = [xs'' | xs' <- subs xs, xs'' <- perms xs']

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- 3. 関数 split を拡張して、組の中に空リストも許すようにすると、
--    関数 solutions の挙動にどのような影響を与えるか説明してください。
split' :: [a] -> [([a], [a])]
split' [] = []
split' [x] = [([], [x]), ([x], [])]
split' (x : xs) = ([], x : xs) : [(x : ls, rs) | (ls, rs) <- split' xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e
  | (ls, rs) <- split ns,
    l <- exprs ls,
    r <- exprs rs,
    e <- combine l r
  ]

-- split'の返すリストが短くならず無限ループになる
exprs' :: [Int] -> [Expr]
exprs' [] = []
exprs' [n] = [Val n]
exprs' ns =
  [ e
  | (ls, rs) <- split' ns,
    l <- exprs' ls,
    r <- exprs' rs,
    e <- combine l r
  ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
  | (ls, rs) <- split ns,
    lx <- results ls,
    ry <- results rs,
    res <- combine' lx ry
  ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e, m) <- results ns', m == n]

-- 2. 再帰的な関数 isChoice :: Eq a => [a] -> [a] -> Bool を定義してください。
--    この関数は、perms や subs を使わずに、一方のリストが他方のリストから
--    選択されたものかを検査します。
--    ヒント：手始めに、あるリストに対して最初に見つかった特定の値を
--    取り除く関数を定義しましょう。
removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (x' : xs)
  | x == x' = xs
  | otherwise = x' : removeFirst x xs

isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice xs (y : ys) = isChoice (removeFirst y xs) ys

-- 解答例
isChoice' [] _ = True
isChoice' (x : xs) [] = False
isChoice' (x : xs) ys = elem x ys && isChoice' xs (removeFirst x ys)

-- 4. 関数 choices、exprs、eval を用いて、1、3、7、10、25、50 に対する
--    可能な式は 33,665,406 個あり、そのうち 4,672,540 個のみが有効である
--    ことを確かめてください。
allExprsLength :: [Int] -> Int
allExprsLength xs = length $ concatMap exprs (choices xs)

validExprsLength :: [Int] -> Int
validExprsLength xs = length $ concatMap eval $ concatMap exprs (choices xs)

validExprsLengthOptimized :: [Int] -> Int
validExprsLengthOptimized xs = length [e | ns <- choices xs, e <- exprs ns, not (null (eval e))]

-- 5. 同様に、数値の定義域を整数に拡大すると有効な式の数が 10,839,369 個に
--    増えることを確かめてください。
--    ヒント：関数 valid の定義を変更しましょう。

-- 解答
-- 以下のように変更しvalidExprsLengthを実行

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub _ _ = True
-- valid Mul _ _ = True
-- valid Div x y = y /= 0 && x `mod` y == 0

-- eval :: Expr -> [Int]
-- eval (Val n) = [n]
-- eval (App o l r) =
--   [ apply o x y
--   | x <- eval l,
--     y <- eval r,
--     valid o x y
--   ]
