data Expr = Val Int | Add Expr Expr | Mul Expr Expr

-- value :: Expr -> Int
-- value (Val n) = n
-- value (Add x y) = value x + value y

type Cont = [Op]

data Op
  = EVAL Expr (Int -> Int -> Int) -- 「次に式を評価して、(Int->Int->Int) で合成する」
  | APPLY (Int -> Int -> Int) Int -- 「前の値 Int と今回の値 Int で (Int->Int->Int) を使い合成」

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y (+) : c)
eval (Mul x y) c = eval x (EVAL y (*) : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y op : c) n = eval y (APPLY op n : c)
exec (APPLY op n : c) m = exec c (op n m)

value :: Expr -> Int
value e = eval e []

-- 抽象機械を拡張して、乗算を扱えるようにしてください。

main :: IO ()
main = do
  -- (2 * 3) + 4
  let expr1 = Add (Mul (Val 2) (Val 3)) (Val 4)
  putStrLn $ "Evaluating (2 * 3) + 4 => " ++ show (value expr1)
  -- 上の例が正しく動くなら 2*3=6, 6+4=10

  -- (1 + 2) * (3 + 4)
  let expr2 = Mul (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))
  putStrLn $ "Evaluating (1 + 2) * (3 + 4) => " ++ show (value expr2)

-- 1+2=3, 3+4=7, 3*7=21
