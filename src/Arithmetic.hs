{-# LANGUAGE FlexibleInstances #-}

module Arithmetic where

import Data.Maybe

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
    mean eodItems = getSumOfEodSeries eodItems/((fromIntegral $ length eodItems) * 4)

class SimpleMovingAverage item where
  sma :: item -> Double


sumClosePricePoint :: [EndOfDayData] -> Double
sumClosePricePoint [] = 0
sumClosePricePoint (x:xs) = close x + sumClosePricePoint xs

instance SimpleMovingAverage [EndOfDayData] where
  sma dataPoints = sumClosePricePoint dataPoints/(fromIntegral $ length dataPoints)

class ExponentialMovingAverage item where
  ema :: item -> Int -> Maybe Double

instance ExponentialMovingAverage [EndOfDayData] where
  ema items numberOfDays
      | (length items) < (fromIntegral $ numberOfDays * 2) = Nothing
      | otherwise = Just $ innerEma series initialEma (emaMultiplier numberOfDays)
          where
            series = (drop (numberOfDays+1) (drop ((length items) - numberOfDays*2) items))
            initialEma = (sma (take numberOfDays (drop ((length items) - numberOfDays*2) items)))
            

emaMultiplier :: Int -> Double
emaMultiplier numberOfDays = 2/(fromIntegral $ 1 + numberOfDays)

innerEma :: [EndOfDayData] -> Double -> Double -> Double
innerEma (x:[]) previousEma multiplier = (multiplier * close x) + (previousEma * (1 - multiplier))
innerEma (x:xs) previousEma multiplier = innerEma xs ((multiplier * close x) + (previousEma * (1 - multiplier))) multiplier