{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits  n 
  | n < 1 = []
  | n < 10 = [n]
  | otherwise  = lastDigit(n) : toRevDigits(dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) factors
  where factors = 1:2:factors

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toRevDigits)
--sumDigits l = sum [sum(toRevDigits(i)) | i <- l]

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
--luhn i = ( sumDigits doubleEveryOther toRevDigits(i) )  `mod` 10 == 0
--luhn = toRevDigits . doubleEveryOther . sumDigits
--luhn = sumDigits . doubleEveryOther .  toRevDigits
luhn = (\x -> x `mod` 10 == 0 ) . sumDigits . doubleEveryOther . toRevDigits
--luhn = undefined

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start goal spare
  | n == 1 = [(start, goal)]
  | otherwise = hanoi (n-1) start spare goal ++ hanoi 1 start goal spare
    ++ hanoi (n-1) spare goal start

