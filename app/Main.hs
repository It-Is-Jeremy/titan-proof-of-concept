module Main (main) where

main :: IO ()
main = printDefault

printDefault :: IO ()
printDefault = do
  putStrLn "Usage [options...]"
  putStrLn "-e Update Strategies execution"
  putStrLn "-b {stockid} {price} Buy stock for certain price"
  putStrLn "-s {stockid} {price} Buy stock for certain price"