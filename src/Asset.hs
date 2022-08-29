{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module Asset where

import GHC.Generics

import DataPoints

data Asset = Asset {
    id          :: Int,
    name        :: String,
    marketName  :: String,
    dataPoints  :: [EndOfDayData]
} deriving Generic

instance Show Asset where
  show asset = "Asset " ++ show (Asset.id asset) ++ " " ++ name asset ++ " " ++ marketName asset ++ " " ++ show (dataPoints asset) ++ " data points"