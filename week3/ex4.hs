import Data.Char
-- ex4
a = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

b = [z | z <- [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]]

c = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- ex6
-- scalarproduct xs ys = sum [x * y | x <- xs, y <- ys] -- wrong
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys ]

-- ex7

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
        | isLower c = int2let ((let2int c + n) `mod` 26)
        | isUpper c = toUpper (shift n (toLower c))
        | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

ex7 = (encode 13 "Think like a Fundamentalist Code like a Hacker")

-- ex8
ex8 = [(x,y) | x <- [1,2], y <- [1,2]]

-- ex9
ex9 = [x| x <- [1,2,3], y <- [1..x]]

-- ex10
ex10 = sum [x | x <- [1..10], even x]

-- ex11
xs = 1 : [x + 1 | x <- xs]

-- ex12
riffle xs ys = concat [[x,y] | (x,y) <- zip xs ys]
ex12 = riffle [1, 2, 3] [4, 5, 6]
