{-# LANGUAGE ViewPatterns #-}
module Graphics.Svg.XmlParser where

import Control.Applicative( (<$>), (<*>), (<|>) )
import Control.Monad( join )
import Text.XML.Light.Proc( findAttr, elChildren )
import Text.XML.Light.Types( Element( .. )
                           , QName( .. ) )
import qualified Data.Text as T
import Data.Attoparsec.Text( parseOnly, many1 )
import Graphics.Svg.Types
import Graphics.Svg.PathParser
import Graphics.Svg.ColorParser
import Graphics.Svg.CssParser( complexNumber )

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

parseDrawAttributes :: Element -> SvgDrawAttributes
parseDrawAttributes e = SvgDrawAttributes
    { _strokeWidth = read <$> attribute "stroke-width"

    , _strokeColor = join $ attribute "stroke" >>= parse colorParser
    , _strokeLineCap = attribute "stroke-linecap" >>= parseSvgCap
    , _strokeLineJoin = attribute "stroke-linejoin" >>= parseSvgLineJoin
    , _strokeMiterLimit = read <$> attribute "stroke-miterlimit"
    , _fillColor   = join $ attribute "fill" >>= parse colorParser
    , _transform   = attribute "transform" >>= parse transformParser
    , _fillOpacity = read <$> attribute "fill-opacity"
    , _strokeOpacity = read <$> attribute "stroke-opacity"
    }
  where attribute a = attributeFinder a e
        parse p str = case parseOnly p (T.pack str) of
            Left _ -> Nothing
            Right r -> Just r

attributeReal :: String -> Element -> Maybe Float
attributeReal attr e = read <$> attributeFinder attr e

attributeLength :: String -> Element -> Maybe SvgNumber
attributeLength attr e = do
  attrValue <- attributeFinder attr e
  case parseOnly complexNumber (T.pack attrValue) of
    Left msg -> fail msg
    Right v -> return v

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
      where parsedPath = case attributeFinder "d" e of
                Nothing -> []
                Just v -> pathParse $ T.pack v

            pathParse txt = case parseOnly (many1 command) txt of
                Left _ -> []
                Right r -> r


unparse _ = SvgNone

unparseDocument :: Element -> Maybe SvgDocument
unparseDocument e@(nodeName -> "svg") = Just $ SvgDocument 
    { _svgViewBox = (0, 0, w, h)
    , _svgElements = unparse <$> elChildren e
    , _svgWidth = width
    , _svgHeight = height
    }
  where
    attr v = attributeLength v e
    width@(Just w) = (floor <$> attr "width") <|> return 0
    height@(Just h) = (floor <$> attr "height") <|> return 0
unparseDocument _ = Nothing   

