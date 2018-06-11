{-# LANGUAGE OverloadedStrings #-}
module PathParserSpec where

import Data.Attoparsec.Text
import Graphics.Svg.PathParser
import Graphics.Svg.Types
import Linear
import Test.Hspec


spec :: Spec
spec = do
  describe "num" $ do
    it "support shorthand number" $ do
      parseOnly command d `shouldBe` Right p
      where
        d = "M-.10 .10z"
        p = MoveTo OriginAbsolute [V2 (-0.1) 0.10]
