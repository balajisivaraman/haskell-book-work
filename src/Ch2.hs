module Ch2 where

triple x = x * 3

waxOn = x * 5
  where
    x = y ^ 2
    y = z + 8
    z = 7

waxOff x = triple x
