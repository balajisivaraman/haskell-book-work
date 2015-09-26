module Ch5 where

funcIgnoreArgs :: a -> a -> a -> String
funcIgnoreArgs _ _ _ = "Blah!"

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i + (nonsense b)

h :: (Num a, Num b) => a -> b -> b
h _ b = b

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a _ = a

kessel :: (Ord a, Num b) => a -> b -> a
kessel a _ = a

someFunc1 :: a -> a -> a
someFunc1 a _ = a

someFunc2 :: a -> a -> a
someFunc2 _ a = a

someFunc3 :: a -> b -> b
someFunc3 _ b = b

f x y = x + y

someFunc4 :: [a] -> Int -> a
someFunc4 l i = head l

x = 5
y = x + 5
z y = y * 10

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = \a -> f(g(a))
