module Main where

import Lib
import Data.Array
import Data.List

type Value = Int
type Index = (Int, Int)

data Cell = Solved Value
          | Unsolved [Value] deriving Show

newtype Sudoku = Sudoku (Array Index Cell)


instance Show Sudoku where
    show (Sudoku s) = "\n" ++ blocksquare ++ "\n"
        where blocksquare = intercalate "\n\n" [blockrow bi  | bi <- [0..2]]
              blockrow bi = intercalate "\n"   [row (3*bi+i) | i <- [0..2]]
              row i       = intercalate "  "   [triple i j   | j <- [0,3,6]]
              triple i j  = concat $ [cell (s ! (i,j+x)) | x <- [0..2]]
              cell (Solved v) = " " ++ show v
              cell (Unsolved _) = " ."


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


isValid :: Sudoku -> Bool
isValid (Sudoku s) = all cell_valid s 
  where
    cell_valid (Solved _)    = True
    cell_valid (Unsolved []) = False
    cell_valid (Unsolved _)  = True


promote :: Cell -> Cell
promote (Solved x) = Solved x
promote (Unsolved l) | (length l == 1) = Solved $ head l
promote (Unsolved l) | otherwise       = Unsolved l


eliminate :: Value -> Cell -> Cell
eliminate value (Unsolved l) = Unsolved $ delete value l
eliminate _ cell = cell


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
pruneAroundCell s idx value = transformCells s (promote . (eliminate value)) indices 
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




main :: IO ()
main = someFunc


