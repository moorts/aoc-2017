{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

parseChoice :: String -> (Int, Int)
parseChoice s = (toNum $ head s,  toNum $ last s)

toNum :: Char -> Int
toNum 'X' = 1
toNum 'Y' = 2
toNum 'Z' = 3
toNum 'A' = 1
toNum 'B' = 2
toNum 'C' = 3
toNum _ = -1

score1 :: (Int, Int) -> Int
score1 (left, right) = right + (right - left + 1) `mod` 3 * 3

score2 :: (Int, Int) -> Int
score2 (left, strat) = (strat - 1) * 3 + (left + strat) `mod` 3 + 1

solve :: ((Int, Int) -> Int) -> [String] -> Int
solve win = sum . map (win . parseChoice)

day02a :: _ :~> _
day02a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . solve score1
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . solve score2
    }
