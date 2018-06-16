
module Elimination
    (   prune, 
        pruneRepeated, 
        progress
    ) where


import Data.Array
import Data.List
import Sudoku


promote :: Cell -> Cell
promote (Solved x) = Solved x
promote (Unsolved l) | (length l == 1) = Solved $ head l
promote (Unsolved l) | otherwise       = Unsolved l


eliminate :: Value -> Cell -> Cell
eliminate value (Unsolved l) = Unsolved $ delete value l
eliminate value (Solved v) | value == v = Unsolved []
                           | otherwise  = Solved v


rowIndices :: Index -> [Index]
rowIndices (r,c) = [(r, cc) | cc <- [0..8], cc /= c]

colIndices :: Index -> [Index]
colIndices (r,c) = [(rr, c) | rr <- [0..8], rr /= r]

blockIndices :: Index -> [Index]
blockIndices (r_initial, c_initial) = [(r,c) | r <- [r_start..r_end], 
                                               c <- [c_start..c_end], 
                                               r /= r_initial || c /= c_initial]
  where
    r_start = (r_initial `div` 3) * 3
    c_start = (c_initial `div` 3) * 3
    r_end = r_start + 2
    c_end = c_start + 2



pruneAroundCell :: Sudoku -> Index -> Value -> Sudoku
pruneAroundCell s idx value = transformCells s (eliminate value) indices 
  where
    indices = nub $ (rowIndices idx) ++ (colIndices idx) ++ (blockIndices idx)


prune :: Sudoku -> Sudoku
prune s = f s targets
  where
    targets = desudoku s
    f s [] = s
    f s ((r,c,v):rest) = f (pruneAroundCell s (r,c) v) rest


progress :: Sudoku -> Int
progress (Sudoku s) = foldr f 0 s
  where
    f (Unsolved l) acc = acc + length(l)
    f (Solved _) acc = acc

pruneRepeated :: Sudoku -> Sudoku
pruneRepeated s = f s (progress s)
  where
    f s p = if (p' == p) then s' else f s' p'
            where
              s' = mapCells (prune s) promote
              p' = progress s'



