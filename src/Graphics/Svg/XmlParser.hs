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
import Text.XML.Light.Proc( findAttrBy, elChildren, strContent )
import Text.XML.Light.Types( Element( .. )
                           , QName( .. ) )
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Attoparsec.Text( Parser, parseOnly, many1 )
import Codec.Picture( PixelRGBA8( .. ) )
import Graphics.Svg.Types
import Graphics.Svg.PathParser
import Graphics.Svg.ColorParser
import Graphics.Svg.CssTypes( CssDeclaration( .. )
                            , CssElement( .. )
                            , CssRule
                            , findMatchingDeclarations
                            )
import Graphics.Svg.CssParser( complexNumber
                             , num
                             , ruleSet
                             , styleString
                             , unitNumber )

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

xmlUpdateDrawAttr :: (WithSvgDrawAttributes a) => Element -> a -> a
xmlUpdateDrawAttr e svg = svg & drawAttr .~ drawAttr'
  where drawAttr' = xmlUpdate (svg ^. drawAttr) e

xmlUpdateWithDrawAttr
    :: (SvgXMLUpdatable a, WithSvgDrawAttributes a)
    => Element -> a -> a
xmlUpdateWithDrawAttr e svg = xmlUpdate (xmlUpdateDrawAttr e svg) e

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

type CssUpdater =
    SvgDrawAttributes -> [[CssElement]] -> SvgDrawAttributes

cssUniqueNumber :: ASetter SvgDrawAttributes SvgDrawAttributes
                   a (Maybe SvgNumber)
                -> CssUpdater
cssUniqueNumber setter attr ((CssNumber n:_):_) = attr & setter .~ Just n
cssUniqueNumber _ attr _ = attr

cssUniqueFloat :: ASetter SvgDrawAttributes SvgDrawAttributes a Float
               -> CssUpdater
cssUniqueFloat setter attr ((CssNumber (SvgNum n):_):_) =
    attr & setter .~ n
cssUniqueFloat _ attr _ = attr

cssUniqueMayFloat :: ASetter SvgDrawAttributes SvgDrawAttributes a (Maybe Float)
               -> CssUpdater
cssUniqueMayFloat setter attr ((CssNumber (SvgNum n):_):_) =
    attr & setter .~ Just n
cssUniqueMayFloat _ attr _ = attr

cssIdentParser :: ASetter SvgDrawAttributes SvgDrawAttributes a b
               -> Parser b
               -> CssUpdater
cssIdentParser setter parser attr ((CssIdent i:_):_) =
  case parseOnly parser i of
    Left _ -> attr
    Right v -> attr & setter .~ v
cssIdentParser _ _ attr _ = attr

cssIdentStringParser :: ASetter SvgDrawAttributes SvgDrawAttributes a b
                     -> (String -> b) -> CssUpdater
cssIdentStringParser setter f attr ((CssIdent i:_):_) =
    attr & setter .~ f (T.unpack i)
cssIdentStringParser _ _ attr _ = attr

cssUniqueTexture :: ASetter SvgDrawAttributes SvgDrawAttributes
                    a (Maybe SvgTexture)
                 -> CssUpdater
cssUniqueTexture setter attr ((CssColor c:_):_) =
    attr & setter .~ Just (ColorRef c)
cssUniqueTexture _ attr _ = attr

cssMayStringSetter :: ASetter SvgDrawAttributes SvgDrawAttributes a (Maybe String)
                   -> CssUpdater
cssMayStringSetter setter attr ((CssIdent i:_):_) =
    attr & setter .~ Just (T.unpack i)
cssMayStringSetter setter attr ((CssString i:_):_) =
    attr & setter .~ Just (T.unpack i)
cssMayStringSetter _ attr _ = attr

drawAttributesList :: [(String, Updater SvgDrawAttributes, CssUpdater)]
drawAttributesList =
    [("stroke-width", numericMaySetter strokeWidth, cssUniqueNumber strokeWidth)
    ,("stroke", parserSetter strokeColor textureParser,
        cssUniqueTexture strokeColor)
    ,("stroke-linecap",
        \e s -> e & strokeLineCap .~ parseSvgCap s,
        cssIdentStringParser strokeLineCap parseSvgCap)
    ,("stroke-linejoin", 
        \e s -> e & strokeLineJoin .~ parseSvgLineJoin s,
        cssIdentStringParser strokeLineJoin parseSvgLineJoin)
    ,("stroke-miterlimit", numMaySetter strokeMiterLimit,
        cssUniqueMayFloat strokeMiterLimit)
    ,("fill", parserSetter fillColor textureParser,
        cssUniqueTexture fillColor)
     -- TODO: fix this incompletness
    ,("transform", parserMaySetter transform (many transformParser), const)
    ,("opacity", numSetter fillOpacity, cssUniqueFloat fillOpacity)
    ,("stroke-opacity", numSetter strokeOpacity, cssUniqueFloat strokeOpacity)
    ,("font-size", numMaySetter fontSize, cssUniqueMayFloat fontSize)
    ,("fill-rule", \e s -> e & fillRule .~ parseFillRule s,
        cssIdentStringParser fillRule parseFillRule)
    ,("class", \e s -> e & attrClass .~ Just s, cssMayStringSetter attrClass)
    ,("id", \e s -> e & attrId .~ Just s, cssMayStringSetter attrId)
    ]

instance SvgXMLUpdatable SvgDrawAttributes where
  defaultSvg = mempty
  svgAttributes =
    stylePrepend [(attrName, updater)
                    | (attrName, updater, _) <- drawAttributesList]
    where stylePrepend lst = ("style", styleUpdater) : lst

styleUpdater :: SvgDrawAttributes -> String
             -> SvgDrawAttributes 
styleUpdater attrs style = case parse styleString style of
    Nothing -> attrs
    Just decls -> foldl' applyer attrs decls
  where
    cssUpdaters = [(T.pack n, u) | (n, _, u) <- drawAttributesList]
    applyer value (CssDeclaration txt elems) =
        case lookup txt cssUpdaters of
          Nothing -> value
          Just f -> f value elems

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

instance SvgXMLUpdatable (SvgSymbol a) where
  defaultSvg = SvgSymbol defaultSvg
  svgAttributes =
        [("viewBox", parserMaySetter (groupOfSymbol . svgGroupViewBox) viewBox)]


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

instance SvgXMLUpdatable SvgUse where
  defaultSvg = defaultSvgUse
  svgAttributes =
    [("x", numericSetter $ svgUseBase._1)
    ,("y", numericSetter $ svgUseBase._2)
    ,("width", numericMaySetter svgUseWidth)
    ,("height", numericMaySetter svgUseHeight)
    ,("href", \e s -> e & svgUseName .~ dropSharp s)
    ]
    where
      dropSharp ('#':rest) = rest
      dropSharp a = a

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
            

data SvgSymbols = SvgSymbols
    { svgSymbols :: !(M.Map String SvgElement)
    , cssStyle   :: [CssRule]
    }

emptyState :: SvgSymbols
emptyState = SvgSymbols mempty mempty
 
parseGradientStops :: Element -> [SvgGradientStop]
parseGradientStops = concatMap unStop . elChildren
  where
    unStop e@(nodeName -> "stop") = [xmlUnparse e]
    unStop _ = []

withId :: Element -> (Element -> SvgElement)
       -> State SvgSymbols SvgTree
withId el f = case attributeFinder "id" el of
  Nothing -> return SvgNone
  Just elemId -> do
      modify $ \s ->
        s { svgSymbols = M.insert elemId (f el) $ svgSymbols s }
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
  Symbol e -> Symbol $ f e
  Group e -> Group $ f e
  Path e -> Path $ f e
  Circle e -> Circle $ f e
  PolyLine e -> PolyLine $ f e
  Polygon e -> Polygon $ f e
  Ellipse e -> Ellipse $ f e
  Line e -> Line $ f e
  Rectangle e -> Rectangle $ f e


unparse :: Element -> State SvgSymbols SvgTree
unparse e@(nodeName -> "style") = do
  case parseOnly (many1 ruleSet) . T.pack $ strContent e of
    Left _ -> return ()
    Right rules ->
      modify $ \s -> s { cssStyle = cssStyle s ++ rules }
  return SvgNone
unparse e@(nodeName -> "defs") = do
    mapM_ unparseDefs $ elChildren e
    return SvgNone
unparse e@(nodeName -> "symbol") = do
  svgChildren <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone svgChildren
  pure . Symbol $ groupNode & svgGroupChildren .~ realChildren
  where
    groupNode = _groupOfSymbol $ xmlUnparseWithDrawAttr e
    isNotNone SvgNone = False
    isNotNone _ = True

unparse e@(nodeName -> "g") = do
  svgChildren <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone svgChildren
  pure . Group $ xmlUnparseWithDrawAttr e & svgGroupChildren .~ realChildren
  where
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
unparse e@(nodeName -> "use") = do
  let useInfo = xmlUnparse e
  svgElem <- gets $ M.lookup (_svgUseName useInfo) . svgSymbols
  case svgElem of
    Nothing -> pure SvgNone
    Just (ElementLinearGradient _) -> pure SvgNone
    Just (ElementRadialGradient _) -> pure SvgNone
    Just (ElementGeometry g) ->
      pure . Use useInfo $ xmlUpdateDrawAttr e g

unparse _ = pure SvgNone

cssDeclApplyer :: SvgDrawAttributes -> CssDeclaration
             -> SvgDrawAttributes 
cssDeclApplyer value (CssDeclaration txt elems) = 
   case lookup txt cssUpdaters of
     Nothing -> value
     Just f -> f value elems
  where
    cssUpdaters = [(T.pack n, u) | (n, _, u) <- drawAttributesList]

cssApply :: [CssRule] -> SvgTree -> SvgTree
cssApply rules = zipSvgTree go where
  go [] = SvgNone
  go ([]:_) = SvgNone
  go context@((t:_):_) = t & drawAttr .~ attr'
   where
     matchingDeclarations =
         findMatchingDeclarations rules context
     attr = view drawAttr t
     attr' = foldl' cssDeclApplyer attr matchingDeclarations
   

unparseDocument :: Element -> Maybe SvgDocument
unparseDocument e@(nodeName -> "svg") = Just $ SvgDocument 
    { _svgViewBox =
        attributeFinder "viewBox" e >>= parse viewBox
    , _svgElements = cssApply (cssStyle named) <$> svgElements
    , _svgWidth = floor <$> lengthFind "width"
    , _svgHeight = floor <$> lengthFind "height"
    , _svgDefinitions = svgSymbols named
    }
  where
    (svgElements, named) =
        runState (mapM unparse $ elChildren e) emptyState
    lengthFind n =
        attributeFinder n e >>= parse unitNumber
unparseDocument _ = Nothing   

