{-# LANGUAGE FlexibleInstances #-}

module Arithmetic where

import DataPoints

class Mean item where
   mean :: item -> Double

class Sum item where
  sum :: item -> Double

instance Sum EndOfDayData where
  sum (EndOfDayData _ _ _ open high low close _) = open + high + low + close

instance Mean EndOfDayData where
  mean eodData = (Arithmetic.sum eodData) / 4

getSumOfEodSeries :: [EndOfDayData] -> Double
getSumOfEodSeries (x:xs) = Arithmetic.sum x + getSumOfEodSeries xs
getSumOfEodSeries [] = 0

instance Mean [EndOfDayData] where
  mean eodItems = (getSumOfEodSeries eodItems)/((fromIntegral $ length eodItems) * 4)