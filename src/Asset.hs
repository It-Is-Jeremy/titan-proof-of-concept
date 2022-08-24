{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module Asset where

import GHC.Generics

import DataPoints

data Asset = Asset {
    id          :: Integer,
    name        :: String,
    marketName  :: String,
    dataPoints  :: [EndOfDayData]
} deriving Generic