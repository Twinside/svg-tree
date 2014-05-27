{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.PathParser( transformParser, command ) where

import Control.Applicative( (<$>), (<$)
                          , (<*>), (<*), (*>)
                          , (<|>)
                          )
import Data.Attoparsec.Text
    ( Number( .. )
    , Parser
    , number
    , string
    , skipSpace
    )
import Data.Attoparsec.Combinator( option, sepBy1 )
import Graphics.Svg.Types
import Graphics.Rasterific.Linear( V2( V2 ) )
import Graphics.Rasterific.Transformations

num :: Parser Float
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where toDouble (I i) = fromIntegral i
        toDouble (D d) = d

        doubleNumber = toDouble <$> number

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ()) <* skipSpace

point :: Parser Point
point = V2 <$> num <* commaWsp <*> num

command :: Parser SvgPath
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

matrixParser :: Parser Transformation
matrixParser = string "matrix" *> skipSpace *> string "(" *> skipSpace *> matrixData
  where
    numComma = num <* string "," <* skipSpace
    close = skipSpace <* string ")" <* skipSpace
    matrixData = Transformation
              <$> numComma <*> numComma <*> numComma
              <*> numComma <*> numComma <*> (num <* close)

