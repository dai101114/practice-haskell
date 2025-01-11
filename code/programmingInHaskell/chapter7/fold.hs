sum :: (Num a) => [a] -> a
sum = foldr (+) 0

product :: (Num a) => [a] -> a
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

snoc x xs = xs ++ [x]

reverse :: [a] -> [a]
reverse = foldr snoc []

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

or' :: [Bool] -> Bool
or' = foldl (||) False

and' :: [Bool] -> Bool
and' = foldl (&&) True

length' :: [a] -> Int
length' = foldl (\n _ -> n + 1) 0

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x : xs) []
