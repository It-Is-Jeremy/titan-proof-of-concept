module ArithmeticSpec where

import Test.Hspec

import Arithmetic
import DataPoints

spec :: Spec
spec = do
  describe "mean" $ do
    it "calculates the mean of an end of day data point" $ do
      mean (EndOfDayData 1 1 (Date 24 08 2022) 10 20 30 40 1000)  `shouldBe` 25