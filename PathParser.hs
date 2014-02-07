{-# LANGUAGE OverloadedStrings #-}
module PathParser where

import Data.Char( isSpace )
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
import Data.Attoparsec.Combinator( option, sepBy1, many1 )

type Coord = Double

type Point = (Coord, Coord)

data Origin
    = OriginAbsolute
    | OriginRelative
    deriving (Eq, Show)

data SvgPath
    = MoveTo Origin [Point]
    | LineTo Origin [Point]

    | HorizontalTo  Origin [Coord]
    | VerticalTo    Origin [Coord]

    -- | Cubic vezier
    | CurveTo  Origin [(Point, Point, Point)]
    -- | Cubic bezier
    | SmoothCurveTo  Origin [(Point, Point)]
    -- | Quadratic bezier
    | QuadraticBezier  Origin [(Point, Point)]
    -- | Quadratic bezier
    | SmoothQuadraticBezierCurveTo  Origin [Point]
    | ElipticalArc  Origin [(Coord, Coord, Coord, Coord, Coord, Point)]
    | EndPath
    deriving (Eq, Show)

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

