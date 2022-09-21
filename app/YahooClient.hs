{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module YahooClient where
--https://query1.finance.yahoo.com/v8/finance/chart/NAB.AX?region=AU&lang=en-AU&includePrePost=false&interval=1d&useYfid=true&range=max&corsDomain=au.finance.yahoo.com&.tsrc=finance

import GHC.Generics
import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Aeson
import Data.Maybe
import Data.Text
import Control.Concurrent
import Control.Exception as Ex

import Prelude

import Data.List.Split
import Data.List.Utils (replace)

import DataPoints
import qualified Data.ByteString.Lazy.Char8 as BS

import Asset

data YahooAssetData = YahooAssetData {
  chart :: YahooChart
} deriving (Generic, Show)

data YahooChart = YahooChart {
  result :: [YahooAssetDataPoint],
  error  :: Maybe String
} deriving (Generic, Show)

data YahooAssetDataPoint = YahooAssetDataPoint {
  meta :: YahooAssetMetaData,
  timestamp :: [Int],
  indicators :: YahooAssetIndicators
} deriving (Generic, Show)

data YahooAssetMetaData = YahooAssetMetaData {
  currency                :: String,
  symbol                  :: String,
  exchangeName            :: String,
  instrumentType          :: String,
  firstTradeDate          :: Int,
  regularMarketTime       :: Int,
  gmtoffset               :: Int,
  timezone                :: String,
  exchangeTimezoneName    :: String,
  regularMarketPrice      :: Double,
  chartPreviousClose      :: Double,
  priceHint               :: Int,
  currentTradingPeriod    :: TradingPeriods,
  dataGranularity         :: String,
  range                   :: String,
  validRanges             :: [String]
} deriving (Generic, Show)

data YahooAssetIndicators = YahooAssetIndicators {
    quote       :: [YahooAssetQuote],
    adjclose    :: [YahooAssetAdjClose]
} deriving (Generic, Show)

data YahooAssetAdjClose = YahooAssetAdjClose {
  adjclose      :: [Double]
} deriving (Generic, Show)

data YahooAssetQuote = YahooAssetQuote {
  open        :: [Double],
  volume      :: [Int],
  low         :: [Double],
  high        :: [Double],
  close       :: [Double]
} deriving (Generic, Show)


data TradingPeriods = TradingPeriods {
  pre       :: TradingPeriod,
  regular   :: TradingPeriod,
  post      :: TradingPeriod
} deriving (Generic, Show)

data TradingPeriod = TradingPeriod {
  timezone  :: String,
  start     :: Int,
  end       :: Int,
  gmtoffset :: Int
} deriving (Generic, Show)

instance FromJSON YahooAssetData
instance FromJSON YahooChart
instance FromJSON YahooAssetDataPoint
instance FromJSON YahooAssetMetaData
instance FromJSON YahooAssetIndicators
instance FromJSON YahooAssetQuote
instance FromJSON YahooAssetAdjClose
instance FromJSON TradingPeriods
instance FromJSON TradingPeriod


retrieveAssetWithWait :: AsxListedCompany -> IO Asset
retrieveAssetWithWait company = do
  asset <- getAsset company `Ex.catch` handler
  threadDelay 2500000
  return asset

handler :: HttpException -> IO Asset
handler exception = do
  return $ Asset 0 "N/A" "N/A" "" []

mapResponseToAsset :: BS.ByteString -> Asset
mapResponseToAsset resp = do
  let response = fromJust $ decode resp :: YahooAssetData
  Asset (regularMarketTime $ meta $ Prelude.head $ result $ chart $ response) (symbol $ meta $ Prelude.head $result $ chart $ response) (exchangeName $ meta $ Prelude.head $ result $ chart $ response) (currency $ meta $ Prelude.head $  result $ chart $ response) (mapResponseToDataPoints $ response)

mapResponseToDataPoints :: YahooAssetData -> [EndOfDayData]
mapResponseToDataPoints response = do
  let quoteData = Prelude.head $ quote $ indicators $ Prelude.head $ result $ chart response
  Prelude.zipWith3 (\x y z -> (EndOfDayData 0 0 (Date 0 0 0) x y z 0 0)) (YahooClient.open quoteData) (YahooClient.high quoteData) (YahooClient.low quoteData)

getAsset :: AsxListedCompany -> IO Asset
getAsset company = runReq defaultHttpConfig $ do
  response <- req
                GET
                (https "query1.finance.yahoo.com" /: "v8" /: "finance" /: "chart" /: (
                Data.Text.pack $ (companyCode company) ++ ".AX"))
                NoReqBody
                lbsResponse
                (queryParam "region" (Just "AU" :: Maybe String) <>
                 queryParam "lang" (Just "en-AU" :: Maybe String) <>
                 queryParam "includePrePost" (Just "false" :: Maybe String) <>
                 queryParam "interval" (Just "1d" :: Maybe String) <>
                 queryParam "useYfid" (Just "true" :: Maybe String) <>
                 queryParam "range" (Just "max" :: Maybe String) <>
                 queryParam "corsDomain" (Just "au.finance.yahoo.com" :: Maybe String) <>
                 queryParam ".tsrc" (Just "finance" :: Maybe String))
  liftIO $ print $ mapResponseToAsset $ (responseBody response :: BS.ByteString)
  return $ Asset 0 (companyCode company) "ASX" "" []