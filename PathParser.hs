{-# LANGUAGE OverloadedStrings #-}
module PathParser where

import Data.Char( isSpace )
import Control.Applicative( (<$>), (<*>), (<*), (*>) )
import Data.Attoparsec.Text
    ( Number( .. )
    , Parser
    , number
    , string
    , skipSpace
    )
import Data.Attoparsec.Combinator( option )

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

    | CurveTo  Origin [(Point, Point, Point)]
    | SmoothCurveTo  Origin [(Point, Point)]
    | QuadraticBezier  Origin [(Point, Point)]
    | SmoothQuadraticBezierCurveTo  Origin [Point]
    | ElipticalArc  Origin [(Coord, Coord, Coord, Coord, Coord, Point)]
    | EndPath
    deriving (Eq, Show)

num :: Parser Double
num = skipSpace *> (toDouble <$> number)
  where toDouble (I i) = fromIntegral i
        toDouble (D d) = d

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ())

point :: Parser Point
point = (,) <$> num <* commaWsp <*> num

pointPair :: Parser (Point, Point)
pointPair = (,) <$> point <* commaWsp <*> point

