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

parseInput :: [Char] -> Maybe [Int]
parseInput l = Just $ map (read . pure :: Char -> Int) l

rotate :: Int -> [a] -> [a]
rotate  =  drop <> take

pairs :: Int -> [a] -> [(a, a)]
pairs n l = zip l (rotate n l)

sumPairs :: [Int] -> Int
sumPairs = sum . map fst . filter (uncurry (==)) . pairs 1

sumPairs' :: [Int] -> Int
sumPairs' l = sum . map fst . filter (uncurry (==)) $ pairs (length l `div` 2) l

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = pure . sumPairs
    }

day01b :: _ :~> _
day01b = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = pure . sumPairs'
    }
