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
printResult (Left (FileError path _)) = putStrLn ("Unable to open file " ++ path)
printResult (Left (ParseError)) = putStrLn "Unable to parse file"
printResult (Left (SolveError)) = putStrLn "Unable to solve file"
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


