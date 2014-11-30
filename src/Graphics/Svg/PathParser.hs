{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.PathParser( transformParser
                              , command
                              , viewBox
                              , pointData
                              ) where

import Control.Applicative( (<$>), (<$)
                          , (<*>), (<*), (*>)
                          , (<|>)
                          )
import Data.Scientific( toRealFloat )
import Data.Attoparsec.Text
    ( Parser
    , scientific
    , string
    , skipSpace
    , char
    )
import Data.Attoparsec.Combinator( option
                                 , sepBy
                                 , sepBy1 )
import Graphics.Svg.Types
import Graphics.Rasterific.Linear( V2( V2 ) )
import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Transformations as RT
import qualified Data.Text as T

num :: Parser Float
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where doubleNumber :: Parser Float
        doubleNumber = toRealFloat <$> scientific

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

viewBox :: Parser (Int, Int, Int, Int)
viewBox = (,,,)
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = floor <$> num <* skipSpace

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ()) <* skipSpace

point :: Parser R.Point
point = V2 <$> num <* commaWsp <*> num

pointData :: Parser [R.Point]
pointData = point `sepBy` commaWsp

command :: Parser Path
command =  (MoveTo OriginAbsolute <$ string "M" <*> pointList)
       <|> (MoveTo OriginRelative <$ string "m" <*> pointList)
       <|> (LineTo OriginAbsolute <$ string "L" <*> pointList)
       <|> (LineTo OriginRelative <$ string "l" <*> pointList)
       <|> (HorizontalTo OriginAbsolute <$ string "H" <*> coordList)
       <|> (HorizontalTo OriginRelative <$ string "h" <*> coordList)
       <|> (VerticalTo OriginAbsolute <$ string "V" <*> coordList)
       <|> (VerticalTo OriginRelative <$ string "v" <*> coordList)
       <|> (CurveTo OriginAbsolute <$ string "C" <*> manyComma curveToArgs)
       <|> (CurveTo OriginRelative <$ string "c" <*> manyComma curveToArgs)
       <|> (SmoothCurveTo OriginAbsolute <$ string "S" <*> pointPairList)
       <|> (SmoothCurveTo OriginRelative <$ string "s" <*> pointPairList)
       <|> (QuadraticBezier OriginAbsolute <$ string "Q" <*> pointPairList)
       <|> (QuadraticBezier OriginRelative <$ string "q" <*> pointPairList)
       <|> (SmoothQuadraticBezierCurveTo OriginAbsolute <$ string "T" <*> pointList)
       <|> (SmoothQuadraticBezierCurveTo OriginRelative <$ string "t" <*> pointList)
       <|> (ElipticalArc OriginAbsolute <$ string "A" <*> manyComma elipticalArgs)
       <|> (ElipticalArc OriginRelative <$ string "a" <*> manyComma elipticalArgs)
       <|> (EndPath <$ string "Z")
       <|> (EndPath <$ string "z")
    where pointList = point `sepBy1` commaWsp
          pointPair = (,) <$> point <* commaWsp <*> point
          pointPairList = pointPair `sepBy1` commaWsp
          coordList = num `sepBy1` commaWsp
          curveToArgs = (,,) <$> (point <* commaWsp)
                             <*> (point <* commaWsp)
                             <*> point
          manyComma a = a `sepBy1` commaWsp

          numComma = num <* commaWsp
          elipticalArgs = (,,,,,) <$> numComma
                                  <*> numComma
                                  <*> numComma
                                  <*> numComma
                                  <*> numComma
                                  <*> point


transformParser :: Parser Transformation
transformParser = matrixParser
               <|> translationParser
               <|> scaleParser
               <|> rotateParser
               <|> skewYParser
               <|> skewXParser

functionParser :: T.Text -> Parser [Float]
functionParser funcName =
    string funcName *> skipSpace
                    *> char '(' *> skipSpace
                    *> num `sepBy1` commaWsp
                    <* skipSpace <* char ')' <* skipSpace

translationParser :: Parser Transformation
translationParser = do
  args <- functionParser "translate"
  return $ case args of
    [x] -> Translate x 0
    [x, y] -> Translate x y
    _ -> TransformUnknown

skewXParser :: Parser Transformation
skewXParser = do
  args <- functionParser "skewX"
  return $ case args of
    [x] -> SkewX x
    _ -> TransformUnknown

skewYParser :: Parser Transformation
skewYParser = do
  args <- functionParser "skewY"
  return $ case args of
    [x] -> SkewY x
    _ -> TransformUnknown


scaleParser :: Parser Transformation
scaleParser = do
  args <- functionParser "scale"
  return $ case args of
    [x] -> Scale x Nothing
    [x, y] -> Scale x (Just y)
    _ -> TransformUnknown

matrixParser :: Parser Transformation
matrixParser = do
  args <- functionParser "matrix"
  return $ case args of
    [a, b, c, d, e, f] ->
        TransformMatrix $ RT.Transformation a b c d e f
    _ -> TransformUnknown

rotateParser :: Parser Transformation
rotateParser = do
  args <- functionParser "rotate"
  return $ case args of
    [angle] -> Rotate angle Nothing
    [angle, x, y] -> Rotate angle $ Just (x, y)
    _ -> TransformUnknown
{-
rotate(<rotate-angle> [<cx> <cy>]), which specifies a rotation by <rotate-angle> degrees about a given point.

If optional parameters <cx> and <cy> are not supplied, the rotation is about the origin of the current user coordinate system. The operation corresponds to the matrix [cos(a) sin(a) -sin(a) cos(a) 0 0].

If optional parameters <cx> and <cy> are supplied, the rotation is about the point (cx, cy). The operation represents the equivalent of the following specification: translate(<cx>, <cy>) rotate(<rotate-angle>) translate(-<cx>, -<cy>).

skewX(<skew-angle>), which specifies a skew transformation along the x-axis.

skewY(<skew-angle>), which specifies a skew transformation along the y-axis.
    -}
