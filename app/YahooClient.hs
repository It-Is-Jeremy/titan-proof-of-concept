{-# LANGUAGE OverloadedStrings #-}

module YahooClient where
--https://query1.finance.yahoo.com/v8/finance/chart/NAB.AX?region=AU&lang=en-AU&includePrePost=false&interval=1d&useYfid=true&range=max&corsDomain=au.finance.yahoo.com&.tsrc=finance

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

import Asset

retrieveAssetWithWait :: AsxListedCompany -> IO Asset
retrieveAssetWithWait company = do
  asset <- getAsset company `Ex.catch` handler
  threadDelay 2500000
  return asset

handler :: HttpException -> IO Asset
handler exception = do
  return $ Asset 0 "N/A" "N/A" "" []

getAsset :: AsxListedCompany -> IO Asset
getAsset company = runReq defaultHttpConfig $ do
  response <- req
                GET
                (https "query1.finance.yahoo.com" /: "v8" /: "finance" /: "chart" /: (pack $ (companyCode company) ++ ".AX"))
                NoReqBody
                jsonResponse
                (queryParam "region" (Just "AU" :: Maybe String) <>
                 queryParam "lang" (Just "en-AU" :: Maybe String) <>
                 queryParam "includePrePost" (Just "false" :: Maybe String) <>
                 queryParam "interval" (Just "1d" :: Maybe String) <>
                 queryParam "useYfid" (Just "true" :: Maybe String) <>
                 queryParam "range" (Just "max" :: Maybe String) <>
                 queryParam "corsDomain" (Just "au.finance.yahoo.com" :: Maybe String) <>
                 queryParam ".tsrc" (Just "finance" :: Maybe String))

  liftIO $ print (responseBody response :: Value)
  return $ Asset 0 (companyCode company) "ASX" "" []