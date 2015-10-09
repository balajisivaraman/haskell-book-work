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
