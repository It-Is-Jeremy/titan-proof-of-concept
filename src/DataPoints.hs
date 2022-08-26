{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}

module DataPoints (
  EndOfDayData (..),
  Date (..)
) where

import GHC.Generics

data Date = Date {
  day   ::  Integer,
  month ::  Integer,
  year  ::  Integer
} deriving Generic

data EndOfDayData = EndOfDayData {
  id                      :: Integer,
  assetId                 :: Integer,
  date                    :: Date,
  open                    :: Double,
  high                    :: Double,
  low                     :: Double,
  close                   :: Double,
  volume                  :: Integer
} deriving Generic
