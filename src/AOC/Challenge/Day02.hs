{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Prelude
import Data.List (tails)

diffRow :: [Int] -> Int
diffRow l = abs (maximum l - minimum l)

solve :: [[Int]] -> Int
solve = sum . map diffRow

pairs :: [Int] -> [(Int, Int)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys, x `mod` y == 0 || y `mod` x == 0]

divide :: Int -> Int -> Int
divide a b
    | a >= b = a `div` b
    | otherwise = b `div` a

quotRow :: [Int] -> Int
quotRow = (uncurry divide) . head . pairs

solve' :: [[Int]] -> Int
solve' = sum . map quotRow

day02a :: [[Int]] :~> Int
day02a = MkSol
    { sParse = pure . map (map read . words) . lines
    , sShow  = show
    , sSolve = pure . solve
    }

day02b :: [[Int]] :~> Int
day02b = MkSol
    { sParse = pure . map (map read . words) . lines
    , sShow  = show
    , sSolve = pure . solve'
    }
