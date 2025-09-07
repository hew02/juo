module Juo.Util (
    numOfDigit
) where

numOfDigit :: Integer -> Int
numOfDigit = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds