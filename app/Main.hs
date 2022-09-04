module Main (main) where
import System.Environment
import Data.List
import Data.Maybe

data Action = ExecuteAction | BuyAction | SellAction

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 1 || (length args) == 3
    then performAction $ determineAction (args!!0)
    else printDefault

performAction :: Maybe Action -> IO ()
performAction Nothing = printDefault
performAction (Just ExecuteAction) = putStrLn "ExecuteAction"
performAction (Just BuyAction) = putStrLn "BuyAction"
performAction (Just SellAction) = putStrLn "SellAction"


determineAction :: String -> Maybe Action
determineAction arg | arg == "-e" = Just ExecuteAction
                    | arg == "-b" = Just BuyAction
                    | arg == "-s" = Just SellAction
                    | otherwise = Nothing

printDefault :: IO ()
printDefault = do
  putStrLn "Usage [options...]"
  putStrLn "-e Update Strategies execution"
  putStrLn "-b {stockid} {price} Buy stock for certain price"
  putStrLn "-s {stockid} {price} Buy stock for certain price"