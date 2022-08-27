{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module Strategy where

import GHC.Generics
import Asset

data OrderType = Buy | Sell deriving (Show, Eq)

data Signal = Signal {
  relevantAssetId     :: Int,
  quantity            :: Int,
  signalType          :: OrderType
} deriving (Generic, Show, Eq)


data OrderFulfilmentStatus = Unfulfilled | Fulfilled | Partial

data Order = Order {
  orderType     :: OrderType,
  orderStatus   :: OrderFulfilmentStatus,
  assetName     :: String,
  orderQuantity :: Int,
  pricePoint    :: Double
} deriving Generic

data Holding = Holding {
  assetId     :: Int,
  marketName  :: String,
  holdingQuantity    :: Int,
  buyPrice    :: Double
} deriving Generic



executeStrategy :: [Asset] -> [Holding] -> Double -> Maybe [Signal]
executeStrategy assets holdings availableBalance = Nothing

