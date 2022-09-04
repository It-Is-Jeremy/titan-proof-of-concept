module ArithmeticSpec where

import Test.Hspec

import Arithmetic
import DataPoints
import Asset
import Strategy

createEodWithPriceOf :: Double -> Double -> Double -> Double -> EndOfDayData
createEodWithPriceOf open high low close = (EndOfDayData 1 1 (Date 24 08 2022) open high low close 1000)

getEodExample :: EndOfDayData
getEodExample = createEodWithPriceOf 10 20 30 40

getEodSeries  :: [EndOfDayData]
getEodSeries = [
  (createEodWithPriceOf 40 40 40 40),
  (createEodWithPriceOf 40 40 40 40),
  (createEodWithPriceOf 40 40 40 40),
  (createEodWithPriceOf 40 40 40 40),
  (createEodWithPriceOf 40 40 40 40),
  (createEodWithPriceOf 20 20 20 20),
  (createEodWithPriceOf 20 20 20 20),
  (createEodWithPriceOf 20 20 20 20),
  (createEodWithPriceOf 20 20 20 20),
  (createEodWithPriceOf 20 20 20 20)]


getStagnantPriceSeriesFor :: Int -> Double -> [EndOfDayData]
getStagnantPriceSeriesFor n price = take n (repeat (EndOfDayData 0 0 (Date 0 0 0) price price price price 10000))

getAsset :: Asset
getAsset = Asset 0 "NAB" "ASX" "" ((getStagnantPriceSeriesFor 400 100) ++ (getStagnantPriceSeriesFor 25 2000))

getHolding :: Holding
getHolding = Holding 0 "ASX" 0 0

spec :: Spec
spec = do
  describe "mean" $ do
    it "calculates the mean of an end of day data point" $ do
      mean getEodExample `shouldBe` 25
    it "calculates the mean price of a series of end of day data points" $ do
      mean getEodSeries `shouldBe` 30
  describe "Simple Moving Average" $ do
    it "calculates the sma of an end of day series" $ do
      sma (map close getEodSeries) `shouldBe` 30
  describe "Exponential Moving Average" $ do
    it "calculates the correct EMA for an example" $ do
      ema [11,12,14,18,12,15,13,16,10] 2 `shouldBe` [11.0,11.666666666666668,13.222222222222221,16.40740740740741,13.469135802469136,14.489711934156379,13.496570644718794,15.165523548239598,11.721841182746534]
    it "calculates the EMA given a series of sufficient length" $ do
      emaForSeries getEodSeries 5 `shouldBe` Just [40.0,33.333333333333336,28.888888888888893,25.92592592592593,23.95061728395062,22.63374485596708]
    it "calculates the correct EMA" $ do
      emaForSeries ((getStagnantPriceSeriesFor 99 100) ++ (getStagnantPriceSeriesFor 1 2000)) 50 `shouldBe` Just [100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,174.50980392156862]
