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
    let d = "M-.10 .10z"
        p = MoveTo OriginAbsolute [V2 (-0.1) 0.10]
    it "support shorthand number" $ do
      parseOnly command d `shouldBe` Right p
  describe "arc" $ do
    let d = "a1.3 1.3 0 01-1.3-1.3"
    --                  ^^ Those two numbers are 2 flags.
    -- This is valid SVG sequence according to https://www.w3.org/TR/SVG/paths.html#PathDataBNF
    -- as flag can be only "0" or "1" and separator is optional.
        p = EllipticalArc OriginRelative [(1.3, 1.3, 0, False, True, V2 (-1.3) (-1.3))]
    it "support flags without separators" $ do
      parseOnly command d `shouldBe` Right p