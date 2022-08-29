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

sma :: [Double] -> Double
sma list = (Prelude.sum list) / (fromIntegral $ length list)

sumClosePricePoint :: [EndOfDayData] -> Double
sumClosePricePoint [] = 0
sumClosePricePoint (x:xs) = close x + sumClosePricePoint xs

emaForSeries :: [EndOfDayData]-> Integer -> Maybe [Double]
emaForSeries eodSeries period | (fromInteger period) > ((length eodSeries)) `div` 2 = Nothing
                              | otherwise = Just $ ema (map close eodSeries) period

ema :: [Double] -> Integer -> [Double]
ema s period = initialEma : innerEma emaSeries period initialEma
  where
     series = s
     emaSeries | period == 2 = drop 1 series
               | otherwise = drop (((fromInteger period)-1 `quot` 2)) series
     initialEma | period == 2 = sma $ take 1 series
                | otherwise = sma $ take (((fromInteger period)-1) `quot` 2) series

innerEma :: [Double] -> Integer -> Double -> [Double]
innerEma [] _ _ = []
innerEma (x:xs) period prevEma = emaValue : innerEma xs period emaValue
  where
    emaValue = x * (a) + ((1 - a)*(prevEma))
    a = (2 / ((fromInteger period) + 1))