{-# LANGUAGE OverloadedStrings #-}

module YahooClient where
--https://query1.finance.yahoo.com/v8/finance/chart/NAB.AX?region=AU&lang=en-AU&includePrePost=false&interval=1d&useYfid=true&range=max&corsDomain=au.finance.yahoo.com&.tsrc=finance

import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Aeson
import Data.Maybe
import Data.Text

import Prelude

import Data.List.Split
import Data.List.Utils (replace)

import Asset

getAsset :: AsxListedCompany -> IO Asset
getAsset company = runReq defaultHttpConfig $ do
  response <- req
                GET
                (https "query1.finance.yahoo.com" /: "v4" /: "finance" /: "chart" /: (pack $ (companyCode company) ++ ".AX"))
                NoReqBody
                jsonResponse
                mempty
  liftIO $ print (responseBody response :: Value)
  return $ Asset 0 (companyCode company) "ASX" "" []