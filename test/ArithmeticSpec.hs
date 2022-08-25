module ArithmeticSpec where

import Test.Hspec

import Arithmetic
import DataPoints

createEodWithPriceOf :: Double -> Double -> Double -> Double -> EndOfDayData
createEodWithPriceOf open high low close = (EndOfDayData 1 1 (Date 24 08 2022) open high low close 1000)

getEodExample :: EndOfDayData
getEodExample = createEodWithPriceOf 10 20 30 40

getEodSeries  :: [EndOfDayData]
getEodSeries = [(createEodWithPriceOf 20 20 20 20), (createEodWithPriceOf 40 40 40 40)]

spec :: Spec
spec = do
  describe "mean" $ do
    it "calculates the mean of an end of day data point" $ do
      mean getEodExample `shouldBe` 25
    it "calculates the mean price of a series of end of day data points" $ do
      mean getEodSeries `shouldBe` 30
