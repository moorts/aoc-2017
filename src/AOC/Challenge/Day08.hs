{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Prelude
import Data.List (findIndex)

col :: Int -> [[Int]] -> [Int]
col j = map (!!j)

row :: Int -> [[Int]] -> [Int]
row i = (!!i)

checkLine :: Int -> [Int] -> Bool
checkLine n line
    | n == length line - 1  || n == 0 = True
    | otherwise = maximum pre < el || maximum post < el
    where (pre, (el:post)) = splitAt n line

isVisible :: Int -> Int -> [[Int]] -> Bool
isVisible y x grid = checkLine y (col x grid) || checkLine x (row y grid)

solve1 :: [[Int]] -> Int
solve1 grid = sum [1 | y <- [0..length grid - 1], x <- [0..length (grid!!0) - 1], isVisible y x grid]

distLine :: Int -> [Int] -> Int
distLine n line
    | n == length line - 1  || n == 0 = 0
    | otherwise = maybe n (+1) (findIndex (>=el) (reverse pre)) * maybe (length line - n - 1) (+1) (findIndex (>=el) post)
    where 
        (pre, (el:post)) = splitAt n line

distance :: Int -> Int -> [[Int]] -> Int
distance y x grid = (distLine y (col x grid)) * (distLine x (row y grid))

solve2 :: [[Int]] -> Int
solve2 grid = maximum [distance y x grid | y <- [0.. length grid -1], x <- [0..length (grid!!0) -1]]

day08a :: _ :~> _
day08a = MkSol
    { sParse = Just . map (map (read . pure)) . lines
    , sShow  = show
    , sSolve = Just . solve1
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just . map (map (read . pure)) . lines
    , sShow  = show
    , sSolve = Just . solve2
    }
