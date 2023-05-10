{-# LANGUAGE OverloadedStrings #-}
module PathParserSpec where

import Data.Attoparsec.Text
import Graphics.Svg.PathParser
import Graphics.Svg.Types
import Linear
import Test.Hspec


spec :: Spec
spec = do
  describe "num" $
    it "support shorthand number" $ do
      let
        d = "M-.10 .10z"
        p = MoveTo OriginAbsolute [V2 (-0.1) 0.10]
      parseOnly command d `shouldBe` Right p
  describe "arc" $ do
    it "support arc" $ do
      let
        d = "a.5 .5 .5 0 0 -.5 .5"
        p = EllipticalArc OriginRelative [(0.5, 0.5, 0.5, False, False, V2 (-0.5) 0.5)]
      parseOnly command d `shouldBe` Right p
    it "support shorthand arc" $ do
      let
        d = "a.5 .5 .5 00-.5 .5"
        p = EllipticalArc OriginRelative [(0.5, 0.5, 0.5, False, False, V2 (-0.5) 0.5)]
      parseOnly command d `shouldBe` Right p
