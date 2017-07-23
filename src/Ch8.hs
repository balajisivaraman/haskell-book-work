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
incTimes times n = 1 + incTimes (times - 1) n

applyTimes :: Integer -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

data DividedResult
  = Result (Integer, Integer)
  | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = Result $ go num denom 0
  where go n d count
          -- | d < 0 = negate(go n (negate d) count)
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

mysum :: Integer -> Integer
mysum 0 = 0
mysum n = n + mysum (n - 1)

main :: IO ()
main = putStrLn "Ch8"
