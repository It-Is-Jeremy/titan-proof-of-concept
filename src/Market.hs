{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}

module Market where

import GHC.Generics

import Asset


data Market = Market {
  id        :: Integer,
  name      :: String,
  assets    :: [Asset]
} deriving Generic