{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.PathParser where

import Data.Bits( (.|.), unsafeShiftR )
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
    , satisfy
    )
import Data.Attoparsec.Combinator( option, sepBy1, many1 )
import Codec.Picture( PixelRGBA8( .. ) )
import Graphics.Svg.NamedColors
import Data.Word( Word8 )
import Graphics.Svg.Types

num :: Parser Double
num = skipSpace *> plusMinus <* skipSpace
  where toDouble (I i) = fromIntegral i
        toDouble (D d) = d

        doubleNumber = toDouble <$> number

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ()) <* skipSpace

point :: Parser Point
point = (,) <$> num <* commaWsp <*> num

pointPair :: Parser (Point, Point)
pointPair = (,) <$> point <* commaWsp <*> point

command :: Parser SvgPath
command =  (MoveTo OriginAbsolute <$ string "M" <*> pointList)
       <|> (MoveTo OriginRelative <$ string "m" <*> pointList)
       <|> (LineTo OriginAbsolute <$ string "L" <*> pointList)
       <|> (LineTo OriginRelative <$ string "l" <*> pointList)
       <|> (HorizontalTo OriginAbsolute <$ string "H" <*> coordList)
       <|> (HorizontalTo OriginRelative <$ string "h" <*> coordList)
       <|> (VerticalTo OriginAbsolute <$ string "V" <*> coordList)
       <|> (VerticalTo OriginRelative <$ string "v" <*> coordList)
       <|> (CurveTo OriginAbsolute <$ string "C" <*> many1 curveToArgs)
       <|> (CurveTo OriginRelative <$ string "c" <*> many1 curveToArgs)
       <|> (SmoothCurveTo OriginAbsolute <$ string "S" <*> pointPairList)
       <|> (SmoothCurveTo OriginRelative <$ string "s" <*> pointPairList)
       <|> (QuadraticBezier OriginAbsolute <$ string "Q" <*> pointPairList)
       <|> (QuadraticBezier OriginRelative <$ string "q" <*> pointPairList)
       <|> (SmoothQuadraticBezierCurveTo OriginAbsolute <$ string "T" <*> pointList)
       <|> (SmoothQuadraticBezierCurveTo OriginRelative <$ string "t" <*> pointList)
       <|> (ElipticalArc OriginAbsolute <$ string "A" <*> many1 elipticalArgs)
       <|> (ElipticalArc OriginRelative <$ string "a" <*> many1 elipticalArgs)
       <|> (EndPath <$ string "Z")
       <|> (EndPath <$ string "z")
    where pointList = point `sepBy1` commaWsp
          pointPair = (,) <$> point <* commaWsp <*> point
          pointPairList = pointPair `sepBy1` commaWsp
          coordList = num `sepBy1` commaWsp
          curveToArgs = (,,) <$> (point <* commaWsp)
                             <*> (point <* commaWsp)
                             <*> point

          numComma = num <* commaWsp
          elipticalArgs = (,,,,,) <$> numComma
                                  <*> numComma
                                  <*> numComma
                                  <*> numComma
                                  <*> numComma
                                  <*> point


transformParser :: Parser Transform
transformParser = string "matrix" *> skipSpace *> string "(" *> skipSpace *> matrixData
  where
    numComma = num <* string ","
    matrixData = Transform <$> numComma <*> numComma <*> numComma
                           <*> numComma <*> numComma <*> num

colorParser :: Parser (Maybe PixelRGBA8)
colorParser = none
           <|> (Just <$> rgbColor)
           <|> (string "#" *> (Just <$> (colorWithAlpha <|> color <|> colorReduced)))
  where
    charRange c1 c2 =
        (\c -> fromIntegral $ fromEnum c - fromEnum c1) <$> satisfy (\v -> c1 <= v && v <= c2)

    hexChar :: Parser Word8
    hexChar = charRange '0' '9'
           <|> ((+ 10) <$> charRange 'a' 'f')
           <|> ((+ 10) <$> charRange 'A' 'F')

    
    percentToWord v = floor $ v * (255 / 100)

    numPercent = ((percentToWord <$> num) <* string "%")
              <|> (floor <$> num)

    hexByte = (\h1 h2 -> h1 `unsafeShiftR` 4 .|. h2) <$> hexChar <*> hexChar
    colorWithAlpha = PixelRGBA8 <$> hexByte <*> hexByte <*> hexByte <*> hexByte
    color = (\r g b -> PixelRGBA8 r g b 255) <$> hexByte <*> hexByte <*> hexByte
    rgbColor = (\r g b -> PixelRGBA8 r g b 255)
            <$> (string "rgb(" *> numPercent)
            <*> (commaWsp *> numPercent)
            <*> (commaWsp *> numPercent <* skipSpace <* string ")")

    colorReduced =
        (\r g b -> PixelRGBA8 (r * 17) (g * 17) (b * 17) 255) <$> hexChar <*> hexChar <*> hexChar
    none = Nothing <$ string "none"
    

