module Backtracking
    (   choose_guess,
        apply_guess,
        solve,
        itersolve
    ) where

import Sudoku
import Elimination


-- For now pick the first unsolved cell with the smallest number of elements
-- Later we can try picking the cell that satisfies the most constraints across the whole grid

choose_guess :: Sudoku -> Maybe (Index, [Value])
choose_guess s = foldl f Nothing [(r,c) | r <- [0..8], c <- [0..8]] 
    where
    f Nothing i = 
        case (getCell s i) of (Unsolved v) -> Just (i, v)
                              (Solved _)   -> Nothing

    f (Just (ii, vv)) i = 
        case (getCell s i) of 
            (Unsolved v) -> if length v < length vv 
                            then Just (i, v)
                            else Just (ii, vv)
            (Solved _) -> Just (ii, vv)


apply_guess :: Sudoku -> Maybe (Index, [Value]) -> [Sudoku]
apply_guess s Nothing          = []
apply_guess s (Just (_, []))   = []
apply_guess s (Just (i, v:vs)) = putCell s i (Solved v) : apply_guess s (Just (i, vs))



-- Do a depth-first search through the solution space
solve :: Sudoku -> Either Error Sudoku
solve s = _solve [s] where
    _solve [] = Left SolveError
    _solve (s:ss) | not (isValid s') = _solve ss
                  | progress s' == 0 = Right s'
                  | otherwise = _solve ((apply_guess s' (choose_guess s')) ++ ss)
        where s' = pruneRepeated s


itersolve :: Sudoku -> [Sudoku]
itersolve s = _solve [s] where
    _solve [] = []
    _solve (s:ss) | not (isValid s') = _solve ss
                  | progress s' == 0 = [s']
                  | otherwise = s : (s' : (_solve ((apply_guess s' (choose_guess s')) ++ ss)))
        where s' = pruneRepeated s


