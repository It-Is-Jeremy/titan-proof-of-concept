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
  assetId     :: Int,
  marketName  :: String,
  holdingQuantity    :: Int,
  buyPrice    :: Double
} deriving Generic


executeStrategy :: [Asset] -> [Holding] -> Double -> Maybe [Signal]
executeStrategy assets holdings availableBalance = Nothing

fiftyDayEma :: [EndOfDayData] -> Maybe Double
fiftyDayEma dataPoints = emaForSeries dataPoints 50

twoHundredDayEma :: [EndOfDayData] -> Maybe Double
twoHundredDayEma dataPoints = emaForSeries dataPoints 200

hasShorterEmaCrossedAboveLongerEma :: Asset -> Bool
hasShorterEmaCrossedAboveLongerEma asset =
  case maybeCurrentShortEma of
    Nothing -> False
    Just shortEma -> case maybeCurrentLongEma of
      Nothing -> False
      Just longEma ->
        case maybePreviousShortEma of
          Nothing -> False
          Just previousShortEma ->
            case maybePreviousLongEma of
              
        previousShortEma < previousLongEma && currentShortEma > currentLongEma
        where
          previousShortEma = take 1 $ drop 2 $
          currentShortEma = drop 1 $ shortEma
          previousLongEma = take 1 $ drop 2 $ longEma
          currentLongEma = drop 1 $ longEma
  where
    maybeCurrentShortEma = fiftyDayEma (dataPoints asset)
    maybeCurrentLongEma = twoHundredDayEma (dataPoints asset)
    maybePreviousShortEma = fiftyDayEma (take (length $ dataPoints asset)-1  (dataPoints asset))
    maybePreviousLongEma = twoHundredDayEma (take (length $ dataPoints asset)-1  (dataPoints asset))

hasShorterEmaCrossedBelowLongerEma :: Asset -> Bool
hasShorterEmaCrossedBelowLongerEma asset = False

