module Playground (   
  fruit_checker
) where

import qualified Data.Map as Map



find_in_file :: String -> IO String
find_in_file user_fruit = do

    fileContents <- readFile "sample.txt"
    let fruits = lines fileContents
    if user_fruit `elem` fruits
    then return "That fruit is in the list."
    else return "No such fruit found!"


fruit_checker :: IO ()
fruit_checker = let
          store_address = ["Flat Street", "18", "New York"]
          pins = Map.fromList [("Mike", 1234), ("Joe", 1111), ("Jack", 2222)]

        in do
          putStrLn $ (store_address !! 0) ++ " " ++ (store_address !! 1)
          putStrLn "Enter your pin code: "
          pinStr <- getLine
          let pin = read pinStr

          if pin `elem` (Map.elems pins)
          then do
              putStrLn "Enter fruit: "
              fruit <- getLine
              result <- find_in_file fruit
              putStrLn result

          else do
              putStrLn "Incorrect pin!"
              putStrLn "This info can be accessed only by: "
              sequence $ map putStrLn (Map.keys pins)
              return ()
