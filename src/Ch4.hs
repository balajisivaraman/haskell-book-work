module Ch4 where

data Mood = Woot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

awesome :: [String]
awesome = ["Papuchon", "curry", "Haskell"]

alsoAwesome :: [String]
alsoAwesome = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = if (reverse list == list) then True else False

myAbs :: Integer -> Integer
myAbs i = if (i < 0) then -i else i

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f ab cd = (((snd ab), (snd cd)), ((fst ab), (fst cd)))

x = (+)

f1 xs = w `x` 1
  where w = length xs
