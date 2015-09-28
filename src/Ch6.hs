module Ch6 where

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i1) (TisAn i2) = i1 == i2

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i3 i4) = (i1 == i3) && (i2 == i4)

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) (TisAString s1) (TisAString s2) = s1 == s2
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair a3 a4) = (a1 == a3) && (a2 == a4)

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) = (a1 == a2) && (b1 == b2)

data Which a =
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a1) (ThisOne a2) = a1 == a2
  (==) (ThatOne a1) (ThatOne a2) = a1 == a2
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a1) (Hello a2) = a1 == a2
  (==) (Goodbye b1) (Goodbye b2) = b1 == b2
  (==) _ _ = False

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

instance Ord DayOfWeek where
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x = if (x == Woot)
               then Blah
               else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

sentence1 :: Object -> Sentence
sentence1 = Sentence "dogs" "drool"

sentence2 :: Sentence
sentence2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- r1 = Papu "chases" True
r2 :: Papu
r2 = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
