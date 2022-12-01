{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Prelude
import Data.List.Split (splitOn)
import Data.List (sort)

paragraphs :: String -> [String]
paragraphs = splitOn "\n\n"

parse :: String -> [[Int]]
parse = map (map read . lines) . paragraphs

solve :: [[Int]] -> Int
solve = maximum . map sum

solve' :: [[Int]] -> Int
solve' = sum . take 3 . reverse . sort . map sum


day01a :: _ :~> _
day01a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve
    }

day01b :: _ :~> _
day01b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve'
    }
