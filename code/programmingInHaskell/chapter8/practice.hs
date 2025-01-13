-- 1. 関数 `add` と同様に、自然数の乗算関数 `mult :: Nat -> Nat -> Nat` を再帰的に定義してください。
--    ヒント: 関数 `add` を使いましょう。
data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ m) n = add n (mult m n)

-- 2. プレリュードには以下のクラスとメソッドが定義されています。
-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering
-- このメソッドを用いて、探索木用の関数 `occurs :: Ord a => a -> Tree a -> Bool` を再定義してください。
-- また、新しい実装が元の実装よりも効率的である理由を説明してください。

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- 旧実装
occurs :: (Ord a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r)
  | x == y = True
  | x < y = occurs x l
  | otherwise = occurs x r

-- 新実装（自分の解答）
occurs' :: (Ord a) => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r)
  | ordering == EQ = True
  | ordering == LT = occurs x l
  | ordering == GT = occurs x r
  where
    ordering = compare x y

-- 新しい実装が元の実装よりも効率的である理由
-- わからない。ほぼ同じに見える。

-- 3. 二分木の定義に基づき、以下を実現してください。
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- すべての節に対して、右と左の部分木にある葉の数が高々一つだけ異なるとき、
-- 木は平衡していると呼びます。
-- 二分木が平衡しているか調べる関数 `balanced :: Tree a -> Bool` を定義してください。
-- ヒント: 最初に木の中の葉の数を返す関数を実装しましょう。
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Show)

t' :: Tree' Int
t' = Node' (Node' (Leaf' 1) (Node' (Leaf' 6) (Node' (Leaf' 6) (Leaf' 9)))) (Node' (Leaf' 6) (Leaf' 9))

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- 4. 空でない整数のリストを平衡木に変換する関数 `balance :: [a] -> Tree a` を定義してください。
--    ヒント: 長さが高々一つだけ異なる二つのリストへ分割する関数を実装してください。
balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance first) (balance second)
  where
    (first, second) = splitAt (length xs `div` 2) xs

-- 5. 以下の型が宣言されています:
data Expr = Val Int | Add Expr Expr deriving (Show)

-- このとき、以下の高階関数を定義してください:
-- `folde f g` は、式の中のそれぞれの `Val` を `f` で置き換え、それぞれの `Add` を `g` で置き換えるとします。
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

-- 6. `folde` を使って以下の関数を定義してください。
-- 例 1: 単一の値
expr1 :: Expr
expr1 = Val 5

-- 例 2: 二つの値の加算
expr2 :: Expr
expr2 = Add (Val 3) (Val 7)

-- 例 3: 入れ子になった加算
expr3 :: Expr
expr3 = Add (Val 2) (Add (Val 1) (Val 4))

-- (1) 式を整数に変換する関数 `eval :: Expr -> Int`
eval :: Expr -> Int
eval = folde id (+)

-- (2) 式の中に整数がいくつあるか数える関数 `size :: Expr -> Int`
size :: Expr -> Int
size = folde (const 1) (+)

data Maybe' a = Nothing' | Just' a

-- 7. 以下のインスタンス宣言を完成させてください。
instance (Eq a) => Eq (Maybe' a) where
  Nothing' == Nothing' = True
  Just' x == Just' y = x == y
  _ == _ = False

-- 新しい型ラッパーの定義
newtype MyList a = MyList [a]

instance (Eq a) => Eq (MyList a) where
  (MyList []) == (MyList []) = True
  (MyList []) == (MyList _) = False
  (MyList _) == (MyList []) = False
  (MyList [x]) == (MyList [y]) = x == y
  (MyList (x : xs)) == (MyList (y : ys)) = x == y && MyList xs == MyList ys
