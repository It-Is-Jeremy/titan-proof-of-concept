{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module Asset where

import GHC.Generics

import DataPoints

data Asset = Asset {
    id          :: Int,
    name        :: String,
    marketName  :: String,
    assetType   :: String,
    dataPoints  :: [EndOfDayData]
} deriving Generic


data AsxListedCompany = AsxListedCompany {
  companyCode                  :: String,
  companyName                  :: String,
  companyDateOpened            :: String,
  companyIndustryType          :: String
} deriving (Eq,Generic, Show)

instance Show Asset where
  show asset = "Asset " ++ show (Asset.id asset) ++ " " ++ name asset ++ " " ++ marketName asset ++ " " ++ show (dataPoints asset) ++ " data points"