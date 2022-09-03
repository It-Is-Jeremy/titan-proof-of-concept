{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module Strategy where

import GHC.Generics
import Asset
import DataPoints
import Arithmetic

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
  assetId             :: Int,
  marketName          :: String,
  holdingQuantity     :: Int,
  buyPrice            :: Double
} deriving Generic

generateBuySignal :: Asset -> Signal
generateBuySignal asset = Signal (Asset.id asset) 2 Buy

generateSellSignal :: Asset -> Signal
generateSellSignal asset = Signal (Asset.id asset) 2 Sell


executeStrategy :: [Asset] -> [Holding] -> Double -> Maybe [Signal]
executeStrategy assets holdings availableBalance
  | (length assets) == 0 = Nothing
  | otherwise = Just $ (map generateBuySignal buySignals) ++ (map generateSellSignal sellSignals)
    where buySignals  = filter hasShorterEmaCrossedAboveLongerEma assets
          sellSignals = filter hasShorterEmaCrossedBelowLongerEma $ filter holdingExists assets
          holdingExists = (\asset -> (Asset.id asset) `elem` (map Strategy.assetId holdings))

fiftyDayEma :: [EndOfDayData] -> Maybe [Double]
fiftyDayEma dataPoints = emaForSeries dataPoints 50

twoHundredDayEma :: [EndOfDayData] -> Maybe [Double]
twoHundredDayEma dataPoints = emaForSeries dataPoints 200

hasShorterEmaCrossedAboveLongerEma :: Asset -> Bool
hasShorterEmaCrossedAboveLongerEma asset =
  case maybeShortEmaSeries of
    Nothing -> False
    Just shortEma -> case maybeLongEmaSeries of
      Nothing -> False
      Just longEma -> (shortEma!!((length shortEma)-1) > longEma!!((length longEma)-1)) && (shortEma!!((length shortEma)-2) <= longEma!!((length longEma)-2))
  where
    maybeShortEmaSeries = fiftyDayEma (dataPoints asset)
    maybeLongEmaSeries = twoHundredDayEma (dataPoints asset)

hasShorterEmaCrossedBelowLongerEma :: Asset -> Bool
hasShorterEmaCrossedBelowLongerEma asset =
    case maybeShortEmaSeries of
      Nothing -> False
      Just shortEma -> case maybeLongEmaSeries of
        Nothing -> False
        Just longEma -> (shortEma!!((length shortEma)-1) < longEma!!((length longEma)-1)) && (shortEma!!((length shortEma)-2) >= longEma!!((length longEma)-2))
    where
      maybeShortEmaSeries = fiftyDayEma (dataPoints asset)
      maybeLongEmaSeries = twoHundredDayEma (dataPoints asset)

