module ArithmeticSpec where

import Test.Hspec

import Arithmetic
import DataPoints

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

spec :: Spec
spec = do
  describe "mean" $ do
    it "calculates the mean of an end of day data point" $ do
      mean getEodExample `shouldBe` 25
    it "calculates the mean price of a series of end of day data points" $ do
      mean getEodSeries `shouldBe` 30
  describe "Simple Moving Average" $ do
    it "calculates the sma of an end of day series" $ do
      sma getEodSeries `shouldBe` 30
  describe "Exponential Moving Average" $ do
    it "returns Nothing when there are insufficient Elements" $ do
      emaForSeries getEodSeries 10 `shouldBe` Nothing
    it "does not return Nothing when there are sufficient Elements" $ do
      emaForSeries getEodSeries 1 `shouldNotBe` Nothing
    it "calculates the EMA given a series of sufficient length" $ do
      emaForSeries getEodSeries 5 `shouldBe` Just 23.95061728395062
