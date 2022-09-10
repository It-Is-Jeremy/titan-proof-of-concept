module YahooClient where
--https://query1.finance.yahoo.com/v8/finance/chart/NAB.AX?region=AU&lang=en-AU&includePrePost=false&interval=1d&useYfid=true&range=max&corsDomain=au.finance.yahoo.com&.tsrc=finance

import Network.HTTP.Req

import Data.Aeson
import Data.Maybe
import Data.Text

import Asset

getAsset :: AsxListedCompany -> IO Asset
getAsset company = do
      response <- runReq defaultHttpConfig $ req
                    GET
                    (https "query1.finance.yahoo.com" /: "v4" /: "finance" /: "chart" /: pack $ (companyCode company) + ".AX")
                    NoReqBody
                    jsonResponse
                    mempty
      return $ Asset 0 (companyCode company) "ASX" "" []