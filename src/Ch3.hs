module Ch3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting = concat[hello, " ", world]

idString :: String -> String
idString s = drop 0 s

getFifthChar :: String -> String
getFifthChar s = take 1 $ drop 4 s

dropNine :: String -> String
dropNine s = drop 9 s

thirdChar :: String -> Char
thirdChar s = s !! 3
