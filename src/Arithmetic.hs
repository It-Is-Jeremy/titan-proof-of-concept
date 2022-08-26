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
  emaForSeries :: item -> Int -> Maybe Double

getSublistForEma :: [EndOfDayData] -> Int -> [EndOfDayData]
getSublistForEma items numberOfDays = drop (length items - numberOfDays*2) items

instance ExponentialMovingAverage [EndOfDayData] where
  emaForSeries items numberOfDays
      | length items < (fromIntegral $ numberOfDays * 2) = Nothing
      | otherwise = generateEma series initialEma $ calculateMultiplier numberOfDays
          where
            series = drop (numberOfDays + 1) $ getSublistForEma items numberOfDays
            initialEma = sma $ take numberOfDays $ getSublistForEma items numberOfDays

calculateMultiplier :: Int -> Double
calculateMultiplier numberOfDays = 2/(fromIntegral $ 1 + numberOfDays)

generateEma :: [EndOfDayData] -> Double -> Double -> Maybe Double
generateEma [] _ _ = Just 0
generateEma (x:[]) previousEma multiplier = Just $ ema x previousEma multiplier
generateEma (x:xs) previousEma multiplier = generateEma xs (ema x previousEma multiplier) multiplier

ema :: EndOfDayData -> Double -> Double -> Double
ema dataPoint prevEma multiplier = multiplier * close dataPoint + prevEma * (1 - multiplier)