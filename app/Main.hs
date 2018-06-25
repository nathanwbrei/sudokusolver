{-# LANGUAGE OverloadedStrings #-}


module Main where

import Sudoku
import Elimination
import Backtracking

import System.IO.Error
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO


easy = sudoku [(0,0,6), (0,1,2), (0,2,8), (0,3,4), (0,5,1), (0,6,3),
               (1,1,5), (1,3,2), (1,4,8), (1,5,3),
               (2,3,6), (2,6,2), (2,7,1),
               (3,2,2), (3,3,5), (3,4,6), (3,5,4), (3,6,1),
               (4,3,1), (4,6,7), (4,7,2), (4,8,4),
               (5,1,4), (5,2,3), (5,3,8), (5,8,6),
               (6,0,4), (6,4,5), (6,5,6), (6,8,2),
               (7,0,5), (7,2,9), (7,4,4), (7,8,1),
               (8,1,3), (8,6,5), (8,7,4), (8,8,7)]


hard = sudoku [(0,7,6),
               (1,1,4), (1,2,5), (1,3,3), (1,6,7),
               (2,3,9), (2,5,6),
               (3,0,2),
               (4,1,7), (4,2,8), (4,4,1), (4,8,5),
               (5,1,1), (5,2,3), (5,3,2),
               (6,1,3), (6,8,4),
               (7,1,8), (7,5,4), (7,6,5),
               (8,2,6), (8,4,2), (8,5,7)]



hard2 = sudoku [(0,1,4), (0,6,6), (0,7,1), (0,8,2),
                (1,1,8), (1,2,2), (1,3,9), (1,6,7), (1,8,4),
                (3,1,7), (3,5,4),
                (4,2,8), (4,3,5), (4,6,3), (4,7,7),
                (5,1,1), (5,2,3),
                (6,3,8),
                (7,2,5), (7,3,1), (7,5,9),
                (8,0,7), (8,4,4), (8,6,1)]



x |> f = f x

parseSudoku :: T.Text -> Either Error Sudoku
parseSudoku t = case parseFile t of 
                  (Right tuples) -> Right $ sudoku tuples
                  (Left err)     -> Left err
  where

    toInts x = x |> T.splitOn "," |> map ((fmap fst) . TR.decimal) |> sequence

    toTuple (Right x) | length x == 3 = Right (x !! 0, x !! 1, x !! 2)
    toTuple (Right x) | otherwise     = Left ParseError
    toTuple (Left x)                  = Left ParseError

    parseLine x = x |> toInts |> toTuple
    parseFile x = x |> T.lines |> (map parseLine) |> sequence


safeReadFile :: FilePath -> IO (Either Error T.Text)
safeReadFile path = catchIOError success failure
  where 
    success = Right <$> (TIO.readFile path)
    failure err = return (Left (FileError path err))


printResult :: (Either Error Sudoku) -> IO ()
printResult (Left err)   = print err
printResult (Right soln) = print soln


main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do  -- IO monad

            contents <- safeReadFile path

            -- TODO: `stack ghci` works but `stack build` fails.
            {- let result = do
                  unpacked <- contents
                  puzzle <- parseSudoku unpacked
                  solve puzzle 
            -}

            let result = contents >>= parseSudoku >>= solve

            printResult result

        otherwise -> 
            putStrLn "Usage: sudokusolver-exe path_to_problem.txt"


