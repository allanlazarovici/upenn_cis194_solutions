{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P pol1) == (P pol2) = dropLeadingZeros pol1 == dropLeadingZeros pol2
      where dropLeadingZeros = dropWhileEnd (== 0) 
 
-- Exercise 3 -----------------------------------------
createTerm :: (String, Int) -> String
createTerm (coeff, deg) = let xpow = (if deg /= 0 then ("x^" ++ show deg) else "")
                              in case coeff of
                                   "1" -> (if deg > 0 then "" else "1") ++ xpow
                                   "-1" -> (if deg > 0 then "-" else "-1") ++ xpow
                                   _ -> coeff ++ xpow

keepNonZeroCoeff :: (String, Int) -> Bool
keepNonZeroCoeff (coeff, _) = coeff /= "0"

coeffsToPoly :: (Num a, Eq a, Show a) => [a] -> [String]
coeffsToPoly a = map createTerm $ filter keepNonZeroCoeff $ zip (map show a) [0..]

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P []) = "0"
  show (P p)  = let ans = intercalate " + " $ reverse $ coeffsToPoly p
                  in if length ans == 0 then "0" else ans

-- Exercise 4 -----------------------------------------
zipWithDefault :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
zipWithDefault _ _ [] [] = []
zipWithDefault f d [] (z:zs) = (f d z) : zipWithDefault f d [] zs
zipWithDefault f d (y:ys) [] = (f y d) : zipWithDefault f d ys []
zipWithDefault f d (y:ys) (z:zs) = (f y z) : zipWithDefault f d ys zs
  
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = P $ zipWithDefault (+) 0 p1 p2

-- Exercise 5 -----------------------------------------
--This code is pretty ugly. I later realized that this program
--can be implemented more elegantly with recursion
genPol :: Num a => Int -> Poly a -> Poly a -> Poly a
genPol i (P p1) (P p2)= shiftPol i $ scalePol (P p1) (P p2) i
  where shiftPol :: Num a => Int -> Poly a -> Poly a
        shiftPol 0 (P p) = (P p)
        shiftPol j (P p) = shiftPol (j-1) (P ([0] ++ p))
        scalePol :: Num a => Poly a -> Poly a -> Int -> Poly a
        scalePol (P p3) (P p4) j = P (map (*p3!!j) p4)

genPols :: Num a => Poly a -> Poly a -> [Poly a]
genPols (P p1) (P p2) = map (\i -> genPol i (P p1) (P p2)) [0..(length p1) - 1]
  
times :: Num a => Poly a -> Poly a -> Poly a
times p1 p2 = foldr (+) (P [0]) (genPols p1 p2)


-- Exercise 6 -----------------------------------------
myNegate :: Num a => Poly a -> Poly a
myNegate p1 = times (P [-1]) p1

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = myNegate
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
--I don't fully understand the Haskell type system
--The function below generates a warning message
applyP (P cs) xval = sum $ map (uncurry (*)) $ zip cs $ map (xval^) [0..]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 f = deriv f
    nderiv n f = nderiv (n-1) (deriv f)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
-- I don't fully understand the Haskell type system
-- I think a function like the one below should work
--  deriv (P p:ps) = P (zipWith (*) ps [1..])
  deriv = undefined

