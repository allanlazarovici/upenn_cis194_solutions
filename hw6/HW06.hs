{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor()
-- import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons s ss) = s : (streamToList ss)

-- Exercise 4 -----------------------------------------

-- fmap (a -> b) -> f a -> f b
instance Functor Stream where
    fmap f (Cons s ss) = Cons (f s) (fmap f ss)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a $ sIterate f $ f a

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons s ss1) ss2 = Cons s $ sInterleave ss2 ss1

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake i (Cons s ss) = s : sTake (i - 1) ss

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = let helper i = sInterleave (sRepeat i) $ helper $ i + 1
        in helper 0

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand i = sIterate f $ f i
  where f = (\x -> (1103515245*x + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = helper x x xs
  where helper cur_min cur_max [] = Just (cur_min, cur_max)
        helper cur_min cur_max (y:ys)
          | y < cur_min = helper y cur_max ys
          | y > cur_max = helper cur_min y ys
          | otherwise = helper cur_min cur_max ys

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
-- This question is listed as optional in the assignment
fastFib :: Int -> Integer
fastFib = undefined
