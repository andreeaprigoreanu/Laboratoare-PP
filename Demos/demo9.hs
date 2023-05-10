{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Maybe
import Data.List (sort, insertBy)
import Data.Function (on)

{-
    Polimorfism parametric -> nicio constrangere pentru 'a'
-}
length_ :: [a] -> Int
length_ [] = 0
length_ (_:xs) = 1 + length_ xs

{-
    Polimorfism ad-hoc -> ocnstrangere ca 'a' sa fie membru al clasei Eq
-}
elem_ :: Eq a => a -> [a] -> Bool
elem_ _ []       = False
elem_ x (y:ys)   = x == y || elem_ x ys

{-
    Polimorfism combinat
        -> nicio constrangere pentru b  => polimorfism parametric
        -> constrangere pentru 'a'      => polimorfism ad-hoc
-}
lookup_ :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup_ _key []         =  Nothing
lookup_  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup_ key xys


------------------------------------------------------- CLASE --------------------------------------------------

data Person = Person {firstName :: String, lastName :: String} deriving (Show, Eq)
p1 = Person "Buddy" "Finklestein"
p2 = Person {firstName = "Buddy", lastName = "Finklestein"}
p3 = Person "Guy" "Smith"

{-
    class  Eq a  where
        (==), (/=) :: a -> a -> Bool
        x /= y = not (x == y)           -> membru implicit
        x == y = not (x /= y)           -> membru implicit
-}

-- instance Eq Person where
--     (==) person1 person2 = firstName person1 == firstName person2 &&
--                            lastName person1 == lastName person2


-- p1 == p2 => True
-- p1 == p3 => False

data BST a = Empty | Node a (BST a) (BST a)

instance Eq a => Eq (BST a) where
    Empty == Empty = True
    Node info1 l1 r1 == Node info2 l2 r2 = info1 == info2 && l1 == l2 && r1 == r2
    _ == _ = False

{-
    Clasa Container reprezintă o clasă folosită pentru enumerarea
    elementelor unei structuri de date (listă, arbore, graf, etc.)
-}

class Container t where
    contents :: t a -> [a]

{-
    Clasa Invertible reprezintă o clasă folosită pentru inversarea
    ordinii de apariție a elementelor unei structuri de date (listă, arbore, etc.)
-}

class Invertible a where
    invert :: a -> a
    invert = id 

data NestedList a = Elem a | List [NestedList a]

nl1 = List [Elem 1, List [List [Elem 2, Elem 3], Elem 4] , Elem 5]
nl2 = List [Elem 1, List [List [Elem 2, Elem 3], Elem 4] , Elem 5]
nl3 = Elem 1

flatten (Elem x)    = [x]
flatten (List xs)   = concatMap flatten xs

instance Eq a => Eq (NestedList a) where
    (==) (Elem a) (Elem b)      = True
    (==) (List xs) (List ys)    = xs == ys
    (==) _ _                    = False

instance Show a => Show (NestedList a) where
    show (Elem x)   = show x
    show (List xs)  = show xs

instance Ord a => Ord (NestedList a) where
    (<=) nl1 nl2 = length (flatten nl1) <= length (flatten nl2)


instance Invertible a => Invertible [a] where
    invert [] = []
    invert (x:xs) = invert xs ++ [invert x]

{-
    invert [1,[[2,3],4],5] = [5,[4,[3,2]],1]
-}

instance Invertible Integer

instance Invertible a => Invertible (NestedList a) where
    invert (Elem x)     = Elem $ invert x
    invert (List xs)    = List $ invert xs

cons :: NestedList a -> NestedList a -> NestedList a
cons xs (Elem x)  = List [xs, Elem x]
cons xs (List ys) = List (xs : ys)

instance Functor NestedList where
    fmap f (Elem x)         = Elem $ f x
    -- fmap f (List [])        = List []
    -- fmap f (List (x:xs))    = (fmap f x) `cons` (fmap f $ List xs)

    fmap f (List xs)        = List $ map (fmap f) xs

instance Container NestedList where
    contents = flatten


-- constante polimorfice --

{-
    5 :: Num p => p
    Numerele sunt constante polimorfice, adica ele pot avea tipuri care sunt instante ale clasei Num (Int, Integer, Float, Double)
    Ne putem gandi ca o constanta este o functie fara parametri
-}

{-
    one :: Constant a => a
    five :: Constant a => a
-}

class Constant a where
  one :: a
  five :: a

instance Constant Int where
  one = 1
  five = 5

instance Constant Float where
  one = 1.0
  five = 5.0

instance Constant String where
  one = "one"
  five = "five"
