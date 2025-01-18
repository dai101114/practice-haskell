import Data.List (sortBy)
import Data.Ord (comparing)

-- 6. 関数 solutions' を以下のように改良してください。
--    a. 式に冪乗演算子が使えるようにする。
--    b. 解がない場合に、目標の数に最も近い解を算出する。
--    c. 適切な方法で解を並べ替える。

data Op = Add | Sub | Mul | Div | Pow

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0
valid Pow x y = x /= 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Pow x y = x ^ y

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

choices :: [a] -> [[a]]
choices = concatMap perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

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

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Pow]

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

-- | 式の大きさを数える例：
--   Val n           -> 1
--   App o left right-> 1(この演算子) + leftのサイズ + rightのサイズ
exprSize :: Expr -> Int
exprSize (Val _) = 1
exprSize (App _ l r) = 1 + exprSize l + exprSize r

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  let allResults =
        [ (e, m)
        | ns' <- choices ns,
          (e, m) <- results ns'
        ]
      diffs = [abs (m - n) | (_, m) <- allResults]
      bestDiff = minimum diffs
      closestRes = [(e, m) | (e, m) <- allResults, abs (m - n) == bestDiff]
      -- (App o l r, m) のリストを “式のサイズ→文字列表現” の順でソート
      sorted = sortBy compareExpr closestRes
   in map fst sorted

-- (式,評価値) の組を比較するための関数
compareExpr :: Result -> Result -> Ordering
compareExpr (e1, _) (e2, _) =
  case compare (exprSize e1) (exprSize e2) of
    EQ -> compare (show e1) (show e2) -- サイズが同じなら文字列表現で比較
    ord -> ord
