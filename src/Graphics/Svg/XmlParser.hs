{-# LANGUAGE ViewPatterns #-}
module Graphics.Svg.XmlParser where

import Control.Applicative( (<$>)
                          {-, (<*>)-}
                          , many )
import Control.Monad( join )
import Text.XML.Light.Proc( findAttr, elChildren )
import Text.XML.Light.Types( Element( .. )
                           , QName( .. ) )
import Data.Maybe( fromMaybe )
import qualified Data.Text as T
import Data.Attoparsec.Text( Parser, parseOnly, many1 )
import Graphics.Svg.Types
import Graphics.Svg.PathParser
import Graphics.Svg.ColorParser
import Graphics.Svg.CssParser( complexNumber, num, unitNumber )

nodeName :: Element -> String
nodeName = qName . elName

attributeFinder :: String -> Element -> Maybe String
attributeFinder str e =
    findAttr QName { qName = str, qURI = Nothing, qPrefix = Nothing }
             e

parseSvgCap :: String -> Maybe SvgCap
parseSvgCap "butt" = Just SvgCapButt
parseSvgCap "round" = Just SvgCapRound
parseSvgCap "square" = Just SvgCapSquare
parseSvgCap _ = Nothing

parseSvgLineJoin :: String -> Maybe SvgLineJoin
parseSvgLineJoin "miter" = Just SvgJoinMiter
parseSvgLineJoin "round" = Just SvgJoinRound
parseSvgLineJoin "bevel" = Just SvgJoinBevel
parseSvgLineJoin _ = Nothing

parse :: Parser a -> String -> Maybe a
parse p str = case parseOnly p (T.pack str) of
  Left _ -> Nothing
  Right r -> Just r

parseMayStartDot :: Parser a -> String -> Maybe a
parseMayStartDot p l@('.':_) = parse p ('0':l)
parseMayStartDot p l = parse p l

parseDrawAttributes :: Element -> SvgDrawAttributes
parseDrawAttributes e = SvgDrawAttributes
    { _strokeWidth = attribute "stroke-width" >>= parse complexNumber
    , _strokeColor = join $ attribute "stroke" >>= parse colorParser
    , _strokeLineCap = attribute "stroke-linecap" >>= parseSvgCap
    , _strokeLineJoin = attribute "stroke-linejoin" >>= parseSvgLineJoin
    , _strokeMiterLimit = attribute "stroke-miterlimit" >>= parseMayStartDot num
    , _fillColor   = join $ attribute "fill" >>= parse colorParser
    , _transform   = attribute "transform" >>= parse (many transformParser)
    , _fillOpacity =
         oneDefault $ attribute "opacity" >>= parseMayStartDot num
    , _strokeOpacity = 
         oneDefault $ attribute "stroke-opacity" >>= parseMayStartDot num
    , _fontSize = attribute "font-size" >>= parseMayStartDot num
    }
  where attribute :: String -> Maybe String
        attribute a = attributeFinder a e

        oneDefault = fromMaybe 1

attributeReal :: String -> Element -> Maybe Float
attributeReal attr e = read <$> attributeFinder attr e

attributeLength :: String -> Element -> Maybe SvgNumber
attributeLength attr e = 
  attributeFinder attr e >>= parseMayStartDot complexNumber

unparse :: Element -> SvgTree
unparse e@(nodeName -> "g") =
    Group (parseDrawAttributes e) $ unparse <$> (elChildren e)
unparse e@(nodeName -> "ellipse") =
    Ellipse (parseDrawAttributes e) c (attr "rx") (attr "ry")
  where
    attr v = fromMaybe (SvgNum 0) $ attributeLength v e
    c = toSvgPoint (attr "cx") (attr "cy")

unparse e@(nodeName -> "rect") =
     Rectangle (parseDrawAttributes e) c
            (attr "width") (attr "height")
            (attr "rx") (attr "ry")
  where
    attr v = fromMaybe (SvgNum 0) $ attributeLength v e
    c = toSvgPoint (attr "x") (attr "y")

unparse e@(nodeName -> "polyline") =
    PolyLine (parseDrawAttributes e) polyData
  where
    polyData =
        fromMaybe [] $ do
          pointString <- attributeFinder "points" e
          parse pointData $ pointString

unparse e@(nodeName -> "polygon") =
    Polygon (parseDrawAttributes e) polyData
  where
    polyData =
        fromMaybe [] $ do
          pointString <- attributeFinder "points" e
          parse pointData $ pointString

unparse e@(nodeName -> "circle") =
    Circle (parseDrawAttributes e) c (attr "r")
  where
    attr v = fromMaybe (SvgNum 0) $ attributeLength v e
    c = toSvgPoint (attr "cx") (attr "cy")

unparse e@(nodeName -> "line") =
    Line (parseDrawAttributes e) p1 p2
  where
    attr v = fromMaybe (SvgNum 0) $ attributeLength v e
    p1 = toSvgPoint (attr "x1") (attr "y1")
    p2 = toSvgPoint (attr "x2") (attr "y2")

unparse e@(nodeName -> "path") =
    Path (parseDrawAttributes e) parsedPath
      where parsedPath = fromMaybe [] $
              attributeFinder "d" e >>= parse (many1 command)

unparse _ = SvgNone

unparseDocument :: Element -> Maybe SvgDocument
unparseDocument e@(nodeName -> "svg") = Just $ SvgDocument 
    { _svgViewBox =
        attributeFinder "viewBox" e >>= parse viewBox
    , _svgElements = unparse <$> elChildren e
    , _svgWidth = floor <$> lengthFind "width"
    , _svgHeight = floor <$> lengthFind "height"
    }
  where
    lengthFind n =
        attributeFinder n e >>= parse unitNumber
unparseDocument _ = Nothing   

