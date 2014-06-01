{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Svg.XmlParser where

import Control.Applicative( (<$>)
                          {-, (<*>)-}
                          , many
                          , pure )

import Control.Lens hiding( transform )
import Control.Monad.State.Strict( State, runState, modify, gets )
import Data.Monoid( mempty )
import Data.List( foldl' )
import Text.XML.Light.Proc( findAttrBy, elChildren )
import Text.XML.Light.Types( Element( .. )
                           , QName( .. ) )
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Attoparsec.Text( Parser, parseOnly, many1 )
import Codec.Picture( PixelRGBA8( .. ) )
import Graphics.Svg.Types
import Graphics.Svg.PathParser
import Graphics.Svg.ColorParser
import Graphics.Svg.CssParser( complexNumber, num, unitNumber )

{-import Debug.Trace-}

nodeName :: Element -> String
nodeName = qName . elName

attributeFinder :: String -> Element -> Maybe String
attributeFinder str =
    findAttrBy (\a -> qName a == str)

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

parseGradientUnit :: String -> GradientUnits
parseGradientUnit "userSpaceOnUse" = GradientUserSpace
parseGradientUnit "objectBoundingBox" = GradientBoundingBox
parseGradientUnit _ = GradientBoundingBox

parseGradientSpread :: String -> SvgSpread
parseGradientSpread "pad" = SpreadPad
parseGradientSpread "reflect" = SpreadReflect
parseGradientSpread "repeat" = SpreadRepeat
parseGradientSpread _ = SpreadPad

parseFillRule :: String -> Maybe SvgFillRule
parseFillRule "nonzero" = Just SvgFillNonZero
parseFillRule "evenodd" = Just SvgFillEvenOdd
parseFillRule _ = Nothing

parse :: Parser a -> String -> Maybe a
parse p str = case parseOnly p (T.pack str) of
  Left _ -> Nothing
  Right r -> Just r

parseMayStartDot :: Parser a -> String -> Maybe a
parseMayStartDot p l@('.':_) = parse p ('0':l)
parseMayStartDot p l = parse p l

xmlUpdate :: (SvgXMLUpdatable a) => a -> Element -> a
xmlUpdate initialSvg el = foldl' grab initialSvg svgAttributes
  where
    grab value (attributeName, setter) =
        case attributeFinder attributeName el of
          Nothing -> value
          Just v -> setter value v

xmlUnparse :: (SvgXMLUpdatable a) => Element -> a
xmlUnparse = xmlUpdate defaultSvg

xmlUnparseWithDrawAttr
    :: (SvgXMLUpdatable a, WithSvgDrawAttributes a)
    => Element -> a
xmlUnparseWithDrawAttr e =
    xmlUnparse e & drawAttr .~ xmlUnparse e

xmlUpdateWithDrawAttr
    :: (SvgXMLUpdatable a, WithSvgDrawAttributes a)
    => Element -> a -> a
xmlUpdateWithDrawAttr e svg = svg' & drawAttr .~ drawAttr'
  where svg' = xmlUpdate svg e
        drawAttr' = xmlUpdate (svg ^. drawAttr) e

attributeReal :: String -> Element -> Maybe Float
attributeReal attr e = read <$> attributeFinder attr e

attributeLength :: String -> Element -> Maybe SvgNumber
attributeLength attr e = 
  attributeFinder attr e >>= parseMayStartDot complexNumber

type Updater t = t -> String -> t

numericSetter :: ASetter a a b SvgNumber -> Updater a
numericSetter setter el str =
  case parseMayStartDot complexNumber str of
    Nothing -> el
    Just v -> el & setter .~ v

numericMaySetter :: ASetter a a b (Maybe SvgNumber) -> Updater a
numericMaySetter setter el str =
  case parseMayStartDot complexNumber str of
    Nothing -> el
    Just v -> el & setter .~ Just v

numMaySetter :: ASetter a a b (Maybe Float) -> Updater a
numMaySetter setter el str =
  case parseMayStartDot num str of
    Nothing -> el
    Just v -> el & setter .~ Just v

numSetter :: ASetter a a b Float -> Updater a
numSetter setter el str =
  case parseMayStartDot num str of
    Nothing -> el
    Just v -> el & setter .~ v

parserSetter :: ASetter a a b e -> Parser e -> Updater a
parserSetter setter parser el str =
  case parse parser str of
    Nothing -> el
    Just v -> el & setter .~ v

parserMaySetter :: ASetter a a b (Maybe e) -> Parser e -> Updater a
parserMaySetter setter parser el str =
  case parse parser str of
    Nothing -> el
    Just v -> el & setter .~ Just v

class SvgXMLUpdatable a where
  svgAttributes :: [(String, Updater a)]
  defaultSvg :: a

instance SvgXMLUpdatable SvgDrawAttributes where
  defaultSvg = mempty
  svgAttributes =
    [("stroke-width", numericMaySetter strokeWidth)
    ,("stroke", parserSetter strokeColor textureParser)
    ,("stroke-linecap",
        \e s -> e & strokeLineCap .~ parseSvgCap s)
    ,("stroke-linejoin", 
        \e s -> e & strokeLineJoin .~ parseSvgLineJoin s)
    ,("stroke-miterlimit", numMaySetter strokeMiterLimit)
    ,("fill", parserSetter fillColor textureParser)
    ,("transform", parserMaySetter transform (many transformParser))
    ,("opacity", numSetter fillOpacity)
    ,("stroke-opacity", numSetter strokeOpacity)
    ,("font-size", numMaySetter fontSize)
    ,("fill-rule", \e s -> e & fillRule .~ parseFillRule s)
    ]

instance SvgXMLUpdatable SvgRectangle where
  defaultSvg = defaultRectangle
  svgAttributes =
    [("width", numericSetter svgRectWidth)
    ,("height", numericSetter svgRectHeight)
    ,("x", numericSetter (svgRectUpperLeftCorner._1))
    ,("y", numericSetter (svgRectUpperLeftCorner._2))
    ,("rx", numericSetter (svgRectCornerRadius._1))
    ,("ry", numericSetter (svgRectCornerRadius._2))
    ]

instance SvgXMLUpdatable SvgLine where
  defaultSvg = defaultLine
  svgAttributes =
    [("x1", numericSetter $ svgLinePoint1._1)
    ,("y1", numericSetter $ svgLinePoint1._2)
    ,("x2", numericSetter $ svgLinePoint2._1)
    ,("y2", numericSetter $ svgLinePoint2._2)
    ]

instance SvgXMLUpdatable SvgEllipse where
  defaultSvg = defaultEllipse
  svgAttributes =
    [("cx", numericSetter $ svgEllipseCenter._1)
    ,("cy", numericSetter $ svgEllipseCenter._2)
    ,("rx", numericSetter svgEllipseXRadius)
    ,("ry", numericSetter svgEllipseYRadius)
    ]

instance SvgXMLUpdatable SvgCircle where
  defaultSvg = defaultCircle
  svgAttributes =
    [("cx", numericSetter $ svgCircleCenter._1)
    ,("cy", numericSetter $ svgCircleCenter._2)
    ,("r", numericSetter svgCircleRadius)
    ]

instance SvgXMLUpdatable SvgPolygon where
  defaultSvg = defaultPolygon
  svgAttributes =
    [("points", parserSetter svgPolygonPoints pointData)]

instance SvgXMLUpdatable SvgPolyLine where
  defaultSvg = defaultPolyLine
  svgAttributes = 
    [("points", parserSetter svgPolyLinePoints pointData)]

instance SvgXMLUpdatable SvgPathPrim where
  defaultSvg = defaultPathPrim
  svgAttributes =
    [("d", parserSetter svgPathDefinition (many1 command))]

instance SvgXMLUpdatable SvgLinearGradient where
  defaultSvg = defaultLinearGradient
  svgAttributes = 
    [("gradientTransform",
        parserSetter linearGradientTransform (many transformParser))
    ,("gradientUnits",
        \e s -> e & linearGradientUnits .~ parseGradientUnit s)
    ,("spreadMethod",
        \e s -> e & linearGradientSpread .~ parseGradientSpread s)
    ,("x1", numericSetter $ linearGradientStart._1)
    ,("y1", numericSetter $ linearGradientStart._2)
    ,("x2", numericSetter $ linearGradientStop._1)
    ,("y2", numericSetter $ linearGradientStop._2)
    ]

instance SvgXMLUpdatable (SvgGroup a) where
  defaultSvg = defaultGroup
  svgAttributes = []

instance SvgXMLUpdatable SvgRadialGradient where
  defaultSvg = defaultRadialGradient
  svgAttributes =
    [("gradientTransform",
        parserSetter radialGradientTransform (many transformParser))
    ,("gradientUnits",
        \e s -> e & radialGradientUnits .~ parseGradientUnit s)
    ,("spreadMethod",
        \e s -> e & radialGradientSpread .~ parseGradientSpread s)
    ,("cx", numericSetter $ radialGradientCenter._1)
    ,("cy", numericSetter $ radialGradientCenter._2)
    ,("r", numericSetter radialGradientRadius)
    ,("fx", numericMaySetter radialGradientFocusX)
    ,("fy", numericMaySetter radialGradientFocusY)
    ]

gradientOffsetSetter :: SvgGradientStop -> String -> SvgGradientStop
gradientOffsetSetter el str = el & gradientOffset .~ val
  where
    val = case parseMayStartDot complexNumber str of
      Nothing -> 0
      Just (SvgNum n) -> n
      Just (SvgPercent n) -> n
      Just (SvgEm n) -> n

instance SvgXMLUpdatable SvgGradientStop where
    defaultSvg = SvgGradientStop 0 (PixelRGBA8 0 0 0 255)
    svgAttributes =
        [("offset", gradientOffsetSetter)
        ,("stop-color", parserSetter gradientColor colorParser)]
            

type SvgSymbols = M.Map String SvgElement
 
parseGradientStops :: Element -> [SvgGradientStop]
parseGradientStops = concatMap unStop . elChildren
  where
    unStop e@(nodeName -> "stop") = [xmlUnparse e]
    unStop _ = []

withId :: Element -> (Element -> a)
       -> State (M.Map String a) SvgTree
withId el f = case attributeFinder "id" el of
  Nothing -> return SvgNone
  Just elemId -> do
      modify $ M.insert elemId (f el)
      return SvgNone

unparseDefs :: Element -> State SvgSymbols SvgTree
unparseDefs e@(nodeName -> "linearGradient") =
  withId e $ ElementLinearGradient . unparser
  where
    unparser ee =
      (xmlUnparse ee) & linearGradientStops .~ parseGradientStops ee
unparseDefs e@(nodeName -> "radialGradient") =
  withId e $ ElementRadialGradient . unparser
  where
    unparser ee =
      (xmlUnparse ee) & radialGradientStops .~ parseGradientStops ee
unparseDefs e = do
  el <- unparse e
  withId e (const $ ElementGeometry el)

svgTreeModify
    :: (forall a. (SvgXMLUpdatable a, WithSvgDrawAttributes a)
            => a -> a)
    -> SvgTree -> SvgTree
svgTreeModify f v = case v of
  SvgNone -> SvgNone
  Use n t -> Use n t
  Group e -> Group $ f e
  Path e -> Path $ f e
  Circle e -> Circle $ f e
  PolyLine e -> PolyLine $ f e
  Polygon e -> Polygon $ f e
  Ellipse e -> Ellipse $ f e
  Line e -> Line $ f e
  Rectangle e -> Rectangle $ f e


unparse :: Element -> State SvgSymbols SvgTree
unparse e@(nodeName -> "defs") = do
    mapM_ unparseDefs $ elChildren e
    return SvgNone
unparse e@(nodeName -> "g") =
  Group . SvgGroup (xmlUnparse e) . filter isNotNone
     <$> groupChildren
  where
    groupChildren = mapM unparse $ elChildren e
    isNotNone SvgNone = False
    isNotNone _ = True

unparse e@(nodeName -> "ellipse") =
  pure . Ellipse $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "rect") = 
  pure . Rectangle $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "polyline") =
  pure . PolyLine $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "polygon") =
  pure . Polygon $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "circle") =
  pure . Circle $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "line") =
  pure . Line $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "path") =
  pure . Path $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "use") =
  case attributeFinder "href" e of
    Nothing -> return SvgNone
    Just n -> do
      let dropSharp ('#':rest) = rest
          dropSharp a = a

      svgElem <- gets . M.lookup $ dropSharp n
      case svgElem of
        Nothing -> pure SvgNone
        Just (ElementLinearGradient _) -> pure SvgNone
        Just (ElementRadialGradient _) -> pure SvgNone
        Just (ElementGeometry g) ->
          pure . Use n $ svgTreeModify (xmlUpdateWithDrawAttr e) g

unparse _ = pure SvgNone

unparseDocument :: Element -> Maybe SvgDocument
unparseDocument e@(nodeName -> "svg") = Just $ SvgDocument 
    { _svgViewBox =
        attributeFinder "viewBox" e >>= parse viewBox
    , _svgElements = svgElements
    , _svgWidth = floor <$> lengthFind "width"
    , _svgHeight = floor <$> lengthFind "height"
    , _svgDefinitions = named
    }
  where
    (svgElements, named) =
        runState (mapM unparse $ elChildren e) mempty
    lengthFind n =
        attributeFinder n e >>= parse unitNumber
unparseDocument _ = Nothing   

