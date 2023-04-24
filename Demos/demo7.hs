
import Data.List

useful_functions = do
    print $ drop 3 [1,2,3,4,5] -- elimina primele 3 elemente din lista
    print $ takeWhile (< 3) [1,2,3,4,1,2,3,4] -- [1,2]
    print $ dropWhile (< 3) [1,2,3,4,5,1,2,3] -- [3,4,1,2,3,4]
    print $ span (< 3) [1,2,3,4,1,2,3,4]      -- ([1,2],[3,4,1,2,3,4])
    print $ break (>= 3) [1,2,3,4,1,2,3,4]    -- ([1,2],[3,4,1,2,3,4])
    print $ splitAt 3 [1,2,3,4,5]             -- ([1,2,3],[4,5])

------------------------------------------- LIST COMPREHENSIONS ----------------------------------------------
l1 = [x * 2 | x <- [1 .. 10]]
l2 = [x | x <- [0, 2 ..], mod x 3 == 0]
l3 = [ (x, y) | x <- [2, 5, 10], y <- [8, 10, 11]]
l4 = [(x, y) | x <- [1 .. 20], y <- [1 .. 5], mod x 3 == 0]

l5 = [(x,y,z) | x <- [1..3], y <- [1..4], x < y, let z = x + y, odd z]

nouns       = ["hobo","frog","pope"]  
adjectives  = ["lazy","grouchy","scheming"]
l6          = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

-- ex filter
-- "abCDeFgHi" -> "CDGH"
removeLowercase st = [c | c <- st, elem c ['A' .. 'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
-- keep even elements
l7 = [[x | x <- xs, even x] | xs <- xxs]

length' xs = sum [1 | _ <- xs]

------------------------------------------- INFINITE LISTS ---------------------------------------------------
naturals1 = [0..]

naturals2 = iter 0
    where iter x = x : iter (x + 1)

naturals3 = iterate (\x -> x + 1) 0

-- puterile lui 2
powsOfTwo = iterate (*2) 1

-- iterate
iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x : iterate'' f (f x)

-- repeat
repeat' :: a -> [a]  
repeat' x = x : repeat' x

-- cycle
-- take 20 $ cycle [2, 5, 7]

-- repeat
ones = repeat 1
-- intersperse
onesTwos = intersperse 2 ones
-- zipWith
fibs = 0 : 1 : zipWith (+) fibs (tail fibs) -- sirul lui Fibonacci 

-- filter
evens = filter even [0..]

-- zipWith
l8 = zipWith (+) [0..] [1..]
l9 = zipWith (zipWith (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

-- zipWith3
l10 = zipWith3 (\x y z -> x + 2 * y + 3 * z) [1..5] [5..10] [10..15]

------------------------------------------ FUNCTION APPLICATION ($) ------------------------------------------

{- 
    -- left vs right associative
    f a b c = (((f a) b) c)

    ($) :: (a -> b) -> a -> b
    f $ expr = f expr

-}


right_association = do
    print $ sqrt 4 + 2 + 3
    print $ sqrt (4 + 2 + 3)
    print $ sqrt $ 4 + 2 + 3
    print $ sum (filter (> 10) (map (*2) [2..10]))
    print $ sum $ filter (> 10) $ map (*2) [2..10]
    print $ map ($ 3) [(4+), (10*), (^2), sqrt]
    print $ map (*3) [1, 2, 3]

------------------------------------------ FUNCTION COMPOSITION (.) ------------------------------------------
{-
    f . g (x) = f(g(x))

    (.) :: (b -> c) -> (a -> b) -> a -> c
    f . g = \x -> f (g x)

-}
function_composition = do
    print $ map (\x -> (+ 1) ((* 2) x)) [1, 2, 3]
    print $ map ((+ 1) . (* 2)) [1, 2, 3]
    print $ map (\xs -> (+ 1) (sum (tail xs))) [[1, 1, 1], [2, 2, 2], [3, 3, 3]]
    print $ map ((+1) . sum . tail) [[1, 1, 1], [2, 2, 2], [3, 3, 3]]
    print $ map (\xs -> (+ 1) $ sum $ tail xs) [[1, 1, 1], [2, 2, 2], [3, 3, 3]]

square x = x * x
inc x = x + 1
f1 x = inc (square x)
f2 x = inc $ square x
f3 x = inc . square $ x
f4 = inc . square

f = (+ 1)
g = (* 2)
run_differences = do
    print $ (f . g) 2
    print $ f $ g 2
    print $ f . g $ 2

------------------------------------------ POINT-FREE PROGRAMMING --------------------------------------------
sum' :: [Integer] -> Integer
sum' xs = foldl (+) 0 xs

sum'' :: [Integer] -> Integer
sum'' = foldl (+) 0

exampleMap = map (\x->2 * x + 1)
-- exampleMap2 = map ((*2) . (+1)) -- (x + 1) * 2
exampleMap2 = map ((+ 1) . (* 2)) -- 2 * x + 1

-- f4 = inc . square


{-
    concatMap :: (a -> [b]) -> t a -> [b]

    concatMap (\x -> [(x, x * 2, x + 2)]) [1 .. 10]
-}

{-
    flip :: (a -> b -> c) -> b -> a -> c
    flip f x y = f y x

    map :: (a -> b) -> [a] -> [b]
    flip map :: [a] -> (a -> b) -> [b]
-}

flip_example = do
    print $ take 10 (map (*2) [0..])
    print $ take 10 $ map (*2) [0..]
    print $ take 10 $ (flip map) [0..] (*2)
    print $ take 10 $ flip map [0..] (*2)
    let f = flip map [0..]
    print $ take 10 $ f (*3)
