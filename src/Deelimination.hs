


module Deelimination
    (   count, deeliminate, all_indices
    ) where


import Sudoku
import Data.Array

data Multiplicity = None | One Index | Multiple deriving Show


count :: Sudoku -> Value -> [Index] -> Multiplicity

count s value indices = foldl (f s value) None indices
  where
    f s value None idx = case getCell s idx of 

        (Solved v)   | v == value -> One idx
                     | otherwise  -> None

        (Unsolved l) | value `elem` l -> One idx
                     | otherwise  -> None

    f s value (One i) idx = case getCell s idx of 

        (Solved v) | v == value -> Multiple
                   | otherwise  -> One i

        (Unsolved l) | value `elem` l -> Multiple
                   | otherwise  -> One i

    f s value Multiple idx = Multiple



deeliminate :: Sudoku -> Value -> [Index] -> Sudoku
deeliminate s v is = case count s v is of
    One i -> putCell s i (Solved v)
    otherwise -> s


all_indices :: [[Index]]
all_indices = rows ++ cols ++ blocks 
  where
    rows = [[(r,c) | c <- [0..8]] | r <- [0..8]]
    cols = [[(r,c) | r <- [0..8]] | c <- [0..8]]
    blocks = [[(r,c) | r <- [br..br+2], c <- [bc..bc+2]] 
                     | br <- [0,3,6], bc <- [0,3,6]]

{-deprune :: Sudoku -> Sudoku
deprune s = _deprune s all_indices
  where
    _deprune s (is:rest) = _deprune s' rest 
      where
        s' = deeliminate s value 
-}
