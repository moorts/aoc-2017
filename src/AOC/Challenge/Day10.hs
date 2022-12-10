{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Prelude
import Control.Monad.State

registerVals = scanl (+) 1

solve lines 
    | trace (show (vals)) False = undefined
    | otherwise = sum [i * (vals !! (i-1)) | i <- [20,60..220]]
    where vals = registerVals lines

enumerate = zip [0..]

drawLine = map (\(idx, pos) -> if abs (idx - pos) < 2 then '#' else '.')

processLine = drawLine . enumerate

drawCrt idx l
    | idx <= 200 = (processLine (take 40 l)) : drawCrt (idx + 40) (drop 40 l)
    | otherwise = []

solve' = drawCrt 0 . registerVals

parseInstr :: [String] -> [Int]
parseInstr ("noop": _) = [0]
parseInstr ("addx": n : _) = [0, read n]

day10a :: _ :~> _
day10a = MkSol
    { sParse = Just . concat . map parseInstr . map words . lines
    , sShow  = show
    , sSolve = Just . solve
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = Just . concat . map parseInstr . map words . lines
    , sShow  = show
    , sSolve = Just . solve'
    }
