module Sudoku
    ( 
        Value,
        Index,
        Sudoku (Sudoku),
        Cell (Solved, Unsolved),
        sudoku, desudoku,
        getCell, putCell, transformCell, transformCells
    ) where


import Data.Array
import Data.List


type Value = Int
type Index = (Int, Int)

data Cell = Solved Value
          | Unsolved [Value] deriving Show

newtype Sudoku = Sudoku (Array Index Cell)




sudoku :: [(Int,Int,Int)] -> Sudoku
sudoku l = Sudoku $ empty // known
    where empty = listArray ((0,0),(8,8)) (replicate 81 (Unsolved [1..9]))
          known = [((x,y), Solved i) | (x,y,i) <- l]

desudoku :: Sudoku -> [(Int,Int,Int)]
desudoku s = f s indices
  where
    indices = [(r,c) | r <- [0..8], c <- [0..8]]
    f s [] = []
    f s ((r,c):rest) = case getCell s (r,c) of (Solved val) -> (r,c,val) : (f s rest)
                                               otherwise    -> f s rest

isValid :: Sudoku -> Bool
isValid (Sudoku s) = all cell_valid s 
  where
    cell_valid (Solved v)    = value_valid v
    cell_valid (Unsolved []) = False
    cell_valid (Unsolved vs)  = all value_valid vs
    value_valid v = (1 <= v) && (v <= 9)


instance Show Sudoku where
    show (Sudoku s) = "\n" ++ blocksquare ++ "\n"
        where blocksquare = intercalate "\n\n" [blockrow bi  | bi <- [0..2]]
              blockrow bi = intercalate "\n"   [row (3*bi+i) | i <- [0..2]]
              row i       = intercalate "  "   [triple i j   | j <- [0,3,6]]
              triple i j  = concat $ [cell (s ! (i,j+x)) | x <- [0..2]]
              cell (Solved v) = " " ++ show v
              cell (Unsolved _) = " ."




putCell :: Sudoku -> Index -> Cell -> Sudoku
putCell (Sudoku s) (x,y) cell = Sudoku $ s // [((x,y), cell)]

getCell :: Sudoku -> Index -> Cell
getCell (Sudoku s) idx = s ! idx

transformCell :: Sudoku -> (Cell -> Cell) -> Index -> Sudoku
transformCell s f idx = putCell s idx $ f (getCell s idx)

transformCells :: Sudoku -> (Cell -> Cell) -> [Index] -> Sudoku
transformCells s f [] = s
transformCells s f (idx:rest) = transformCells (transformCell s f idx) f rest

mapCells :: Sudoku -> (Cell -> Cell) -> Sudoku
mapCells (Sudoku s) f = Sudoku $ fmap f s





