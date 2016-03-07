module Ch8 where

-- fourFactorial :: Integer
-- fourFactorial = 4 * 3 * 2 * 1

brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1(n - 1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

incOne :: Num a => a -> a
incOne = (+1)

three :: Integer
three = incOne . incOne . incOne $ 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

main :: IO ()
main = putStrLn "Ch8"
