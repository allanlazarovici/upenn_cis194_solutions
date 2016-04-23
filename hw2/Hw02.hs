{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq, Ord)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches x y = length $ filter (\z -> z) $ zipWith (==) x y


-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors input = map (\x -> length $ filter (\y -> y == x) input) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith (min) (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove answer guess = Move guess exact (total - exact)
  where exact = exactMatches answer guess
        total = matches answer guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move code _ _) secret = move == getMove secret code


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (\code -> isConsistent move code)

-- Exercise 6 -----------------------------------------
cartprod :: [Peg] -> [Code] -> [Code]
cartprod cols codes = [[col] ++ code | col <- cols, code <- codes]

allCodes :: Int -> [Code]
allCodes n
  | n == 0 = [[]]
  | otherwise = cartprod colors $ allCodes (n-1)

-- Exercise 7 -----------------------------------------
-- solve [Red, Blue, Green, Yellow]

solve :: Code -> [Move]
solve answer = makeAGuess (allCodes $ length answer)
  where makeAGuess :: [Code] -> [Move]
        makeAGuess [] = []
        makeAGuess (first:rest) = curMove:makeAGuess (filterCodes curMove rest)
          where curMove = getMove answer first

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
