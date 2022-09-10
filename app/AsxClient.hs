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

import Asset

getListedCompanies :: IO [Company]
getListedCompanies = runReq defaultHttpConfig $ do
  response <- req
                GET
                (https "asx.api.markitdigital.com" /: "asx-research" /: "1.0" /: "companies" /: "directory" /: "file")
                NoReqBody
                bsResponse
                mempty
  liftIO $ print (responseStatusCode response :: Int)
  let companies = parseListedCompaniesCSV (responseBody response :: ByteString)
  liftIO $ print $ companies
  liftIO $ print $ Prelude.length companies
  return companies

parseListedCompaniesCSV :: ByteString -> [AsxListedCompany]
parseListedCompaniesCSV string = Prelude.map (fromJust) $ Prelude.filter (/=Nothing) $ Prelude.map (generateCompany) $ Prelude.map (splitOn ",") $ Prelude.map (replace "\"" "")$ Prelude.drop 1 $ splitOn "\n" $ bsToString string

generateCompany :: [String] -> Maybe AsxListedCompany
generateCompany c | (Prelude.length c) == 5 = Just $ AsxListedCompany (c!!0) (c!!1) (c!!2) (c!!3)
                  | otherwise = Nothing

bsToString :: ByteString -> String
bsToString bs = Prelude.map (chr . fromEnum) . Data.ByteString.unpack $ bs