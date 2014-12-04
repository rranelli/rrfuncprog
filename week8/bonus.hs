foldl1 :: (acc -> el -> acc) -> acc -> [el] -> acc
foldl1 f a bs = foldr (\b -> \g -> \a -> g (f a b)) id bs a

foldl2 f a bs = foldr (\a b -> f b a) a bs

foldl3 f = flip $ foldr (\a b g -> b (f g a)) id

foldl4 = foldr . flip
