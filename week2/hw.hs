-- ex0
-- halve xs = (take n xs, drop n xs)
--   where n = (length xs) `div` 2

-- halve xs = splitAt (length xs `div` 2) xs

-- ex1
safetail [] = []
safetail xs = tail xs

--  ex4
mult :: Num a => a -> a -> a -> a
mult = \x -> (\y -> (\z -> x * y * z))
