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

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude
import Data.Char (ord)
import Data.List.Split (splitEvery)
import Data.Set (intersection, fromList)
import qualified Data.Set as M
score :: Char -> Int
score c
    | ord c > ord 'Z' = (ord c - ord 'a') + 1
    | otherwise = 26 + (ord c - ord 'A') + 1

-- Actual Challenge Code
process :: [Char] -> Int
process line = score . head $ filter (\x -> x `elem` right) left
    where (left, right) = splitAt (length line `div` 2) line

-- This one won't work with current parsing (remove map fromList .) to make it work
processGroup :: [[Char]] -> Int
processGroup [first, second, third] = score . head $ filter (\x -> x `elem` second && x `elem` third) first

-- Bit more elegant version using set intersections
processGroup' :: [Set Char] -> Int
processGroup' (x:xs) = score $ M.elemAt 0 $ foldl' intersection x xs


day03a :: _ :~> _
day03a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . map process
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = Just . splitEvery 3 . map fromList . lines
    , sShow  = show
    , sSolve = Just . sum . map processGroup'
    }
