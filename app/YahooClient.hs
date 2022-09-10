module YahooClient where
/*
  https://query1.finance.yahoo.com/v8/finance/chart/NAB.AX?region=AU&lang=en-AU&includePrePost=false&interval=1d&useYfid=true&range=max&corsDomain=au.finance.yahoo.com&.tsrc=finance
*/

import Network.HTTP.Req

import Data.Aeson
import Data.Maybe

import Asset

getAssets :: [AsxListedCompany] -> IO [Asset]
getAssets =

    liftIO $ print (responseStatusCode response :: Int)

getAsset :: AsxListedCompany -> IO [Asset]
getAsset company = runReq defaultHttpConfig $ do
      response <- req
                    GET
                    (https "query1.finance.yahoo.com" /: "v4" /: "finance" /: "chart" /: (companyCode company) + ".AX")
                    NoReqBody
                    jsonResponse
                    mempty
      return Asset 0 (companyCode company) "ASX" ()