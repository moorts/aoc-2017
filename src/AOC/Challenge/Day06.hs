{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Prelude
import qualified Data.List as L

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . L.tails

-- Alternative version:
windows' :: Int -> [a] -> [[a]]
windows' m = map (take m) . L.tails

isMarker :: Int -> [Char] -> Bool
isMarker n = (n==) . length . L.nub

solve' :: Int -> [Char] -> Int
solve' n = (+n) . fromJust . L.findIndex (isMarker n) . windows' n

day06a :: _ :~> _
day06a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . solve' 4
    }

day06b :: _ :~> _
day06b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . solve' 14
    }
