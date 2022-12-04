{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Prelude
import Data.List.Split (splitOn)

parse :: String -> [[[Int]]]
parse = map (\line -> map (map read . splitOn ("-")) (splitOn ","  line)) . lines

solve1 :: [[[Int]]] -> Int
solve1 = length . filter check

solve2 :: [[[Int]]] -> Int
solve2 = length . filter check'

check :: [[Int]] -> Bool
check [[a0, a1], [b0, b1]] = (a0 <= b0 && a1 >= b1) || (a0 >= b0 && a1 <= b1)
check _ = error "Should not be here"

check' :: [[Int]] -> Bool
check' [[a0, a1], [b0, b1]] = not (a1 < b0 || a0 > b1)
check' _ = error "Should not be here"


day04a :: _ :~> _
day04a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve1
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve2
    }
