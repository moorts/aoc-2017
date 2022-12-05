{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Prelude
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Map as M
import Control.Exception (assert)
import Control.Monad.State

type Stacks = M.Map Int [Char]

paragraphs :: String -> [String]
paragraphs = splitOn "\n\n"

enumerate :: [Char] -> [(Int, Char)]
enumerate = zip [1..]

getLetters :: String -> [(Int, Char)]
getLetters = filter ((/= ' ') . snd) . enumerate . map (!! 1) . chunksOf 4

parseCrates :: String -> Stacks
parseCrates = foldl parseLine M.empty . map getLetters . tail . reverse . lines

parseLine :: Stacks -> [(Int, Char)] -> Stacks
parseLine = foldl insertLetter

insertLetter :: Stacks -> (Int, Char) -> Stacks
insertLetter mp (crate, letter)
    | letter == ' ' = mp
    | otherwise = M.insertWith (++) crate [letter] mp

parseProc :: String -> [[Int]]
parseProc = map f . lines
    where f = mapMaybe readMaybe . splitOn " "

parse :: String -> (Stacks, [[Int]])
parse s
--    | trace ("Instrs: " ++ show (parseProc instrs)) False = undefined
    | otherwise = (parseCrates crates, parseProc instrs)
    where
        [crates, instrs] = assert (length pars == 2) pars
        pars = paragraphs s

move :: Int -> Int -> Stacks -> Stacks
move src dst mp
--    | trace ("Map: " ++ show mp) False = undefined
    | otherwise = insertLetter updatedMap (dst, popped)
    where 
        Just val = M.lookup src mp
        popped = head val
        updatedMap = M.adjust tail src mp

pop :: Int -> Stacks -> (Maybe Char, Stacks)
pop src stacks = (head <$> M.lookup src stacks, M.adjust tail src stacks)

popCrates :: Int -> Int -> Stacks -> ([Char], Stacks)
popCrates n src mp = foldl (\(letters, stacks) _ -> let (Just letter, updatedStacks) = pop src stacks in (letter : letters, updatedStacks)) ([], mp) [1..n]

moveN :: [Int] -> Stacks -> Stacks
moveN [n, src, dst] mp = last . take (n+1) $ iterate (move src dst) mp
moveN _ _ = error "Invalid instruction"

moveN' :: [Int] -> Stacks -> Stacks
moveN' [n, src, dst] mp = foldl (\acc letter -> insertLetter acc (dst, letter)) updatedMap letters
    where
        (letters, updatedMap) = popCrates n src mp
moveN' _ _ = error "Invalid instruction"



solve :: (Stacks, [[Int]]) -> [Char]
solve (mp, instrs) = map head . filter (not . null) $ (M.elems $ foldl (flip moveN) mp instrs)

solve' :: (Stacks, [[Int]]) -> [Char]
solve' (mp, instrs) = map head . filter (not . null) $ (M.elems $ foldl (flip moveN') mp instrs)

day05a :: _ :~> _
day05a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve
    }

day05b :: _ :~> _
day05b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve'
    }
