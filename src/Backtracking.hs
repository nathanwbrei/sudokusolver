module Backtracking
    ( 
        choose_guess,
        apply_guess,
        solve
    ) where

import Sudoku

choose_guess :: Sudoku -> (Index, [Value])
choose_guess s = undefined

apply_guess :: (Index, [Value]) -> [Sudoku]
apply_guess (idx, v:vs) = undefined

solve :: Sudoku -> Sudoku
solve s = undefined
