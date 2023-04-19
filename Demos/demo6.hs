
-- Functii

-- http://learnyouahaskell.com/syntax-in-functions

-- Functii
add x y = x + y
add' x = \y -> x + y
add'' = \x -> \y -> x + y
add1 = add 1

-- operatori
x = 1 + 2
x' = (+) 1 2
y = max 2 3
y' = 2 `max` 3

-- (2 +) == \x -> 2 + x
-- (+ 2) == \x -> x + 2
-- (- 2) == -2
-- (2 -) == \x -> 2 - x

-- 5 :: Int
-- 'H' :: Char
-- "Hello" :: [Char] -- lista de Char
-- True :: Bool
-- False :: Bool

-- Int vs Integer

-- Liste
l1 = [1, 3, 5]
l2 = 1:3:5:[]
l3 = [1, 3 .. 8]
l4 = [1, 2 ..]

-- Sringurile sunt liste de Char

l5 = l1 ++ l2
l6 = l2 ++ [10, 10] ++ [11, 12]
l7 = [l1, l2]
-- l7 = [l1, l2] ++ [1, 1, 1]

-- head, tail, append

-- Tuples
t1 = (1, 2)
t2 = (1, "Ana")
t3 = (1, 2, "hello")
tuples = do
    print (fst t1)
    print (snd t2)

-- fst, snd

-- Functionale

-- map, filter, foldl, foldr
functionals_map = do
    print $ map (+ 1) [1, 2, 3]
    print $ map (* 3) [1, 2, 3]
    print $ map (\x -> x + 5) [1, 2, 3]

functionals_filter = do
    print $ filter even [1, 2, 3, 4]
    print $ filter (\x -> x > 2) [1, 2, 3, 4]
    print $ filter (> 2) [1, 2, 3, 4]
    print $ filter (2 >) [1, 2, 3, 4]

functionals_folds = do
    print $ foldl (\acc x -> x : acc) [] [1, 2, 3, 4]
    print $ foldr (\x acc -> x : acc) [] [1, 2, 3, 4]
    print $ zip [1, 2, 3] [4, 5, 6]
    print $ zipWith (+) [1, 2, 3] [4, 5, 6]

-- sintaxa functii

-- 0 -- cu 'if .. then .. else'
fact_else x = if x == 0 then 1 else x * fact_else (x - 1)


-- 1 ------------------------------------------- PATTERN MATCHING -------------------------------------------

factorial 0 = 1
factorial n = n * factorial (n - 1)

length' :: (Num b) => [a] -> b  
length' [] = 0
length' (_:xs) = 1 + length' xs

first :: (a, b, c) -> a
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
 
-- 2 ------------------------------------------------ GUARDS ------------------------------------------------

factorial_guards :: (Num a, Eq a) => a -> a
factorial_guards n
    | n == 0 = 1
    | otherwise = n * factorial_guards (n - 1)


length_guards :: (Num b) => [a] -> b
length_guards l
    | null l = 0
    | otherwise = 1 + length_guards (tail l)


myCompare :: (Ord a) => a -> a -> String  
myCompare a b
    | a > b     = "GT"  
    | a == b    = "EQ"
    | otherwise = "L"


-- 3 ------------------------------------------------ CASE OF ------------------------------------------------

-- case expression of pattern -> result  
--                    pattern -> result  
--                    pattern -> result  
--                    ...  

factorial_case :: (Num a, Eq a) => a -> a
factorial_case n = case n of
    0 -> 1
    _ -> n * factorial_case (n - 1)

length_case l = case l of
    []  -> 0
    _   -> 1 + length_case (tail l)


-- --------------------------------------------------- LET ---------------------------------------------------
-- let <bindings> in <expression>
-- folosit pentru o a introduce un local scope

p1 = let 
    x = y + 1
    y = 2
    f n = if n == 0 then [] else n : f (n - 1) 
    in (x + y, f 2)

p2 = 4 * (let a = 9 in a + 1) + 2
p3 = [let square x = x * x in (square 5, square 3, square 2)]
p4 = (let a = 1; b = 2; c = 3 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

-- -------------------------------------------------- WHERE --------------------------------------------------

p11 = (x + y, f 2)
    where
        x = y + 1
        y = 2
        f n = if n == 0 then [] else n : f (n - 1)

p33 = (square 5, square 3, square 9)
    where
        square x = x * x

p44 = (a*b*c, foo ++ bar)
    where
        a = 1
        b = 2
        c = 3
        foo = "Hey "
        bar = "there!"

-- pattern match where
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list." 

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname 
          (l:_) = lastname
