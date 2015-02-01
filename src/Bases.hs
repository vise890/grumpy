module Bases
( Base
, Digits
, newNumber
, digits
, base
, toBase
) where


type Base = Int
type Digits = [Int]

-- A number represented by digits and a base
data Number = Number Digits Base

-- http://www.mathsisfun.com/base-conversion-method.html
-- https://cs.stackexchange.com/questions/10318/the-math-behind-converting-from-any-base-to-any-base-without-going-through-base

newNumber :: Int -> Base -> Number
newNumber n b = Number (toDigits n b) b

digits :: Number -> Digits
digits (Number ds _) = ds

base :: Number -> Base
base (Number _ b) = b

toBase :: Number -> Base -> Number
toBase (Number ds b) b' = Number ds' b'
                          where ds' = toDigits (fromDigits ds b) b'

fromDigits :: [Int] -> Base -> Int
fromDigits ds b = foldl (\n d -> n*b + d) 0 ds

-- From a platonic number to a Number
toDigits :: Int -> Base -> [Int]
toDigits = toDigits' []
           where toDigits' ds 0 _ = ds
                 toDigits' ds n b = toDigits' (n `mod` b:ds) (n `div` b) b

