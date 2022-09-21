{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module DataPoints (
  EndOfDayData (..),
  Date (..)
) where

import GHC.Generics

data Date = Date {
  day   ::  Int,
  month ::  Int,
  year  ::  Int
} deriving Generic

data EndOfDayData = EndOfDayData {
  id                      :: Int,
  assetId                 :: Int,
  date                    :: Date,
  open                    :: Double,
  high                    :: Double,
  low                     :: Double,
  close                   :: Double,
  volume                  :: Int
} deriving Generic

instance Show EndOfDayData where
  show (EndOfDayData id assetId date open high low close volume) =
    show open