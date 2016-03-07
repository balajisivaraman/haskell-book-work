module Ch7 where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
    Galapagos
  | Antarctic
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives

humboldt  = Peng SouthAmerica
gentoo    = Peng Antarctic
macaroni  = Peng Antarctic
little    = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctic) = True
antarcticPenguin _                = False

antarcticOrGalapagosPenguin :: Penguin -> Bool
antarcticOrGalapagosPenguin p =
  (galapagosPenguin p) || (antarcticPenguin p)

-- Better solution for example in Chapter 4
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

third3 :: (a, b, c) -> c
third3 (_, _, c) = c

f1 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f1 (a, b, c) (d, e, f) = ((a, d), (c, f))

funcZ :: Integer -> String
funcZ x =
  case x + 1 == 1 of
    True  -> "AWESOME"
    False -> "WUT"

functionC :: Integer -> Integer -> Integer
functionC x y =
  case x > y of
    True  -> x
    False -> y

ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 n =
  case even n of
    True  -> n + 2
    False -> n

nums :: Integer -> Integer
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

-- employeeRank :: Employee -> Employee -> IO ()
-- employeeRank e e' =
--   case compare e e' of
--     GT -> reportBoss e e'
--     LT -> (flip reportBoss) e e'
--     EQ -> putStrLn "Neither employee is the boss"

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee
             -> Employee
             -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> flip reportBoss e e'

dodgy :: Integer -> Integer -> Integer
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = flip dodgy 2

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = -x
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135   = "too low"
  | x > 145   = "too high"
  | otherwise = "just right"

numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

myprint :: Show a => a -> IO ()
myprint = putStrLn . show

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst $ x `divMod` 10
        d = snd $ xLast `divMod` 10

hundsDigit :: Integral a => a -> a
hundsDigit x = d
  where xLast = fst $ x `divMod` 100
        d = snd $ xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True  -> x
  False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
  | b   = x
  | not b = y

g :: (a -> b) -> (a, c) -> (b, c)
g f2 (a, c) = (f2 a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTripTypes :: (Show a, Read b) => a -> b
roundTripTypes = read . show

main :: IO ()
main = do
  print (roundTrip 4)
  print (id 4)
{--
main = do
  print (0 :: Int) -- 0
  print (add 1 0) -- 1
  print (addOne 0) -- 1
  print (addOnePF 0) -- 1
  print ((addOne . addOne) 0) -- 2
  print ((addOnePF . addOne) 0) -- 2
  print ((addOne . addOnePF) 0) -- 2
  print ((addOnePF . addOnePF) 0) -- 2
  print (negate (addOne 0)) -- -1
  print ((negate . addOne) 0) -- -1
  print ((addOne . addOne . addOne . negate . addOne) 0) -- 2
--}
