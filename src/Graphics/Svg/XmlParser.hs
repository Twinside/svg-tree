{-# LANGUAGE ViewPatterns #-}
module Graphics.Svg.XmlParser where

import Control.Applicative( (<$>), (<*>) )
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
import Graphics.Svg.CssParser( complexNumber, num )

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

parseDrawAttributes :: Element -> SvgDrawAttributes
parseDrawAttributes e = SvgDrawAttributes
    { _strokeWidth = attribute "stroke-width" >>= parse complexNumber
    , _strokeColor = join $ attribute "stroke" >>= parse colorParser
    , _strokeLineCap = attribute "stroke-linecap" >>= parseSvgCap
    , _strokeLineJoin = attribute "stroke-linejoin" >>= parseSvgLineJoin
    , _strokeMiterLimit = attribute "stroke-miterlimit" >>= parse num
    , _fillColor   = join $ attribute "fill" >>= parse colorParser
    , _transform   = attribute "transform" >>= parse transformParser
    , _fillOpacity = attribute "fill-opacity" >>= parse num
    , _strokeOpacity = attribute "stroke-opacity" >>= parse num
    }
  where attribute :: String -> Maybe String
        attribute a = attributeFinder a e

attributeReal :: String -> Element -> Maybe Float
attributeReal attr e = read <$> attributeFinder attr e

attributeLength :: String -> Element -> Maybe SvgNumber
attributeLength attr e = 
  attributeFinder attr e >>= parse complexNumber

unparse :: Element -> SvgTree
unparse e@(nodeName -> "g") =
    Group (parseDrawAttributes e) $ unparse <$> (elChildren e)
unparse e@(nodeName -> "ellipse") =
    maybe SvgNone id $ Ellipse (parseDrawAttributes e)
                        <$> c <*> attr "rx" <*> attr "ry"
  where
    attr v = attributeLength v e
    c = toSvgPoint <$> attr "cx" <*> attr "cy"

unparse e@(nodeName -> "rect") =
    maybe SvgNone id $ Rectangle (parseDrawAttributes e)
                    <$> c <*> attr "width" <*> attr "height"
  where
    attr v = attributeLength v e
    c = toSvgPoint <$> attr "x" <*> attr "y"

unparse e@(nodeName -> "circle") =
    maybe SvgNone id $ Circle (parseDrawAttributes e)
                        <$> c <*> attr "r"
  where
    attr v = attributeLength v e
    c = toSvgPoint <$> attr "cx" <*> attr "cy"

unparse e@(nodeName -> "line") =
    maybe SvgNone id $ Line (parseDrawAttributes e) <$> p1 <*> p2
  where
    attr v = attributeLength v e
    p1 = toSvgPoint <$> attr "x1" <*> attr "y1"
    p2 = toSvgPoint <$> attr "x2" <*> attr "y2"

unparse e@(nodeName -> "path") =
    Path (parseDrawAttributes e) parsedPath
      where parsedPath = fromMaybe [] $
              attributeFinder "d" e >>= parse (many1 command)


unparse _ = SvgNone

unparseDocument :: Element -> Maybe SvgDocument
unparseDocument e@(nodeName -> "svg") = Just $ SvgDocument 
    { _svgViewBox = Just (0, 0, floor width, floor height)
    , _svgElements = unparse <$> elChildren e
    , _svgWidth = Just $ floor width
    , _svgHeight = Just $ floor height
    }
  where
    lengthFind :: String -> Float
    lengthFind n =
        fromMaybe 0 $ (attributeFinder n e >>= parse num)

    width :: Float
    width = lengthFind "width"
    height = lengthFind "height"
unparseDocument _ = Nothing   

