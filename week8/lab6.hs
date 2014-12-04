------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a
root = \tree -> case tree of
                     root :> _ -> root

children :: Rose a -> [Rose a]
children = \rose -> case rose of
                     _ :> children -> children

ex0 = length $ children ('x' :> map (flip (:>) []) ['a'..'x'])

treez = 'x' :> map (\c -> c :> []) ['a'..'A']
ex1 = length $ children treez

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size = \rose -> case rose of
                 _ :> children -> 1 + sum (map size children)
                 _ :> [] -> 0

leaves :: Rose a -> Int
leaves = \rose -> case rose of
                   _ :> [] -> 1
                   _ :> xs -> sum (map leaves xs)

-- this value is used in so many places...
tree = (1 :> map (\c -> c :> []) [1..5])

ex3 = size tree
ex4 = size . head . children $ 1 :> map (\c -> c :> []) [1..5]
ex5 = leaves $ 1 :> map (\c -> c :> []) [1..5]
ex6 = product (map leaves (children tree))
ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap = \f r -> case r of
                  head :> children -> f head :> map (fmap f) children

tst1 = fmap (*2) (1 :> [2 :> [], 3 :> []])  -- should eq (2 :> [4 :> [], 6 :> []])
tst2 = fmap (+1) (1 :> []) -- should eq (2 :> [])

ex8 = size (fmap leaves (fmap (:> []) tree))
-- ex9 = identity function. You rise the rose into the rose of list of a then
-- rise it into rose of head of list of a which turns out to be the original rose.
-- Some fancy way to define the identity function
ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend = \x y -> Sum $ (unSum x) + (unSum y)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend = \x y -> Product $ (unProduct x) * (unProduct y)

unSum :: Sum a -> a
unSum = \s -> case s of Sum x -> x
unProduct :: Product a -> a
unProduct = \p -> case p of Product x -> x

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))

ex11 = unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))
-- ex12 = its just a sum m'boy
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

tree' = 1 :> [2 :> [], 3 :> [4 :> []]]
tree'' = fmap Product tree'

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  -- foldMap maps each member of the structore to a monoid, and then combine the results
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap = \mapper functor -> fold (fmap mapper functor)

instance Foldable Rose where
  fold = \rose -> case rose of
                   val :> rs -> val `mappend` foldr mappend mempty (map fold rs)

ex14 = unProduct $ fold tree''

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

tree''' = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
ex16 = unSum $ foldMap Sum tree'''

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum = \structure -> unSum $ foldMap (\x -> Sum x) structure
fproduct = \structure -> unProduct $ foldMap (\x -> Product x) structure

ex19 = fsum xs
ex20 = fproduct xs
ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)
