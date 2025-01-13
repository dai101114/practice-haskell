data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Eq Prop Prop
  | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- トートロジーになる例
-- 1. A ∨ ¬A (排中律)
p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

-- 2. A ≡ A (同一性)
p6 :: Prop
p6 = Eq (Var 'A') (Var 'A')

-- 3. (A ∨ B) ≡ (B ∨ A) (交換律)
p7 :: Prop
p7 = Eq (Or (Var 'A') (Var 'B')) (Or (Var 'B') (Var 'A'))

-- トートロジーにならない例
-- 1. A ∨ B (A と B の値による)
p8 :: Prop
p8 = Or (Var 'A') (Var 'B')

-- 2. A ≡ B (A と B の値による)
p9 :: Prop
p9 = Eq (Var 'A') (Var 'B')

-- 3. (A ∨ B) ≡ A (A と B の値による)
p10 :: Prop
p10 = Eq (Or (Var 'A') (Var 'B')) (Var 'A')

type Assoc k v = [(k, v)]

find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Eq p q) = eval s p == eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Eq p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- 恒等式の検査器を拡張して、命題の中で論理和（_）と同値（≡）を扱えるようにしてください。
