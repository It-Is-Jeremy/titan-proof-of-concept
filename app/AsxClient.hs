{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module AsxClient where

import GHC.Generics


import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Aeson
import Data.Maybe

import Data.ByteString
import Data.ByteString.Char8 as C8 (pack)
import Data.Char                   (chr)
import Prelude

import Data.List.Split
import Data.List.Utils (replace)

data Company = Company {
  companyCode                  :: String,
  companyName                  :: String,
  companyDateOpened            :: String,
  companyIndustryType          :: String
} deriving (Eq,Generic, Show)

getListedCompanies :: IO ()
getListedCompanies = runReq defaultHttpConfig $ do
  response <- req
                GET
                (https "asx.api.markitdigital.com" /: "asx-research" /: "1.0" /: "companies" /: "directory" /: "file")
                NoReqBody
                bsResponse
                mempty
  liftIO $ print (responseStatusCode response :: Int)
  liftIO $ print $ parseListedCompaniesCSV (responseBody response :: ByteString)

parseListedCompaniesCSV :: ByteString -> [Company]
parseListedCompaniesCSV string = Prelude.map (fromJust) $ Prelude.filter (/=Nothing) $ Prelude.map (generateCompany) $ Prelude.map (splitOn ",") $ Prelude.map (replace "\"" "")$ Prelude.drop 1 $ splitOn "\n" $ bsToString string

generateCompany :: [String] -> Maybe Company
generateCompany c | (Prelude.length c) == 5 = Just $ Company (c!!0) (c!!1) (c!!2) (c!!3)
                  | otherwise = Nothing

bsToString :: ByteString -> String
bsToString bs = Prelude.map (chr . fromEnum) . Data.ByteString.unpack $ bs