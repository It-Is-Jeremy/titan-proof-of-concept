module StrategySpec where

import Test.Hspec

import Asset
import DataPoints
import Strategy

getStagnantPriceSeriesFor :: Int -> Double -> [EndOfDayData]
getStagnantPriceSeriesFor n price = take n (repeat (EndOfDayData 0 0 (Date 0 0 0) price price price price 10000))

getAsset :: Asset
getAsset = Asset 0 "NAB" "ASX" ((getStagnantPriceSeriesFor 399 100) ++ (getStagnantPriceSeriesFor 1 2000))

getHolding :: Holding
getHolding = Holding 0 "ASX" 0 0

spec :: Spec
spec = do
  describe "strategy test setup" $ do
    it "Generates the required test data" $ do
      length (getStagnantPriceSeriesFor 200 100) `shouldBe` 200
  describe "strategy" $ do
    it "Recognises when 50 day ema is greater than 200 day ema" $ do
      putStrLn (show $ fiftyDayEma $ dataPoints getAsset)
      putStrLn (show $ twoHundredDayEma $ dataPoints getAsset)
      (getAsset) `shouldSatisfy` (hasShorterEmaCrossedAboveLongerEma)
    it "Generates a buy signal when the 50 day ema crosses above the 200 day ema" $ do
      (executeStrategy [(getAsset)] [] 1000) `shouldNotBe` Nothing
    it "Generates a buy signal when the 50 day ema crosses above the 200 day ema" $ do
          (executeStrategy [(getAsset)] [] 1000) `shouldBe` Just [(Signal 0 2 Buy)]