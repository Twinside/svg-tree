{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.XmlParser where

import Control.Applicative( (<$>)
                          {-, (<*>)-}
                          , many
                          , pure )

import Control.Lens hiding( transform, children, elements, element )
import Control.Monad.State.Strict( State, runState, modify, gets )
import Data.Monoid( mempty, Last( Last ) )
import Data.List( foldl' )
import Text.XML.Light.Proc( findAttrBy, elChildren, strContent )
import qualified Text.XML.Light.Types as X
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
                             , dashArray
                             , styleString
                             , numberList )

{-import Debug.Trace-}

nodeName :: X.Element -> String
nodeName = X.qName . X.elName

attributeFinder :: String -> X.Element -> Maybe String
attributeFinder str =
    findAttrBy (\a -> X.qName a == str)

parseCap :: String -> Last Cap
parseCap "butt" = Last $ Just CapButt
parseCap "round" = Last $ Just CapRound
parseCap "square" = Last $ Just CapSquare
parseCap _ = Last Nothing

parseTextAnchor :: String -> Last TextAnchor
parseTextAnchor "middle" = Last $ Just TextAnchorMiddle
parseTextAnchor "start" = Last $ Just TextAnchorStart
parseTextAnchor "end" = Last $ Just TextAnchorEnd
parseTextAnchor _ = Last Nothing

parseLineJoin :: String -> Last LineJoin
parseLineJoin "miter" = Last $ Just JoinMiter
parseLineJoin "round" = Last $ Just JoinRound
parseLineJoin "bevel" = Last $ Just JoinBevel
parseLineJoin _ = Last Nothing

parseGradientUnit :: String -> GradientUnits
parseGradientUnit "userSpaceOnUse" = GradientUserSpace
parseGradientUnit "objectBoundingBox" = GradientBoundingBox
parseGradientUnit _ = GradientBoundingBox

parseGradientSpread :: String -> Spread
parseGradientSpread "pad" = SpreadPad
parseGradientSpread "reflect" = SpreadReflect
parseGradientSpread "repeat" = SpreadRepeat
parseGradientSpread _ = SpreadPad

parseFillRule :: String -> Last FillRule
parseFillRule "nonzero" = Last $ Just FillNonZero
parseFillRule "evenodd" = Last $ Just FillEvenOdd
parseFillRule _ = Last Nothing

parseTextAdjust :: String -> TextAdjust
parseTextAdjust "spacing" = TextAdjustSpacing
parseTextAdjust "spacingAndGlyphs" = TextAdjustSpacingAndGlyphs
parseTextAdjust _ = TextAdjustSpacing

parse :: Parser a -> String -> Maybe a
parse p str = case parseOnly p (T.pack str) of
  Left _ -> Nothing
  Right r -> Just r

parseMayStartDot :: Parser a -> String -> Maybe a
parseMayStartDot p l@('.':_) = parse p ('0':l)
parseMayStartDot p l = parse p l

xmlUpdate :: (XMLUpdatable a) => a -> X.Element -> a
xmlUpdate initial el = foldl' grab initial attributes
  where
    grab value (attributeName, setter) =
        case attributeFinder attributeName el of
          Nothing -> value
          Just v -> setter value v

xmlUnparse :: (XMLUpdatable a) => X.Element -> a
xmlUnparse = xmlUpdate defaultSvg

xmlUnparseWithDrawAttr
    :: (XMLUpdatable a, WithDrawAttributes a)
    => X.Element -> a
xmlUnparseWithDrawAttr e =
    xmlUnparse e & drawAttr .~ xmlUnparse e

xmlUpdateDrawAttr :: (WithDrawAttributes a) => X.Element -> a -> a
xmlUpdateDrawAttr e svg = svg & drawAttr .~ drawAttr'
  where drawAttr' = xmlUpdate (svg ^. drawAttr) e

xmlUpdateWithDrawAttr
    :: (XMLUpdatable a, WithDrawAttributes a)
    => X.Element -> a -> a
xmlUpdateWithDrawAttr e svg = xmlUpdate (xmlUpdateDrawAttr e svg) e

attributeReal :: String -> X.Element -> Maybe Float
attributeReal attr e = read <$> attributeFinder attr e

attributeLength :: String -> X.Element -> Maybe Number
attributeLength attr e = 
  attributeFinder attr e >>= parseMayStartDot complexNumber

type Updater t = t -> String -> t

numericSetter :: ASetter a a b Number -> Updater a
numericSetter setter el str =
  case parseMayStartDot complexNumber str of
    Nothing -> el
    Just v -> el & setter .~ v

numericMaySetter :: ASetter a a b (Maybe Number) -> Updater a
numericMaySetter setter el str =
  case parseMayStartDot complexNumber str of
    Nothing -> el
    Just v -> el & setter .~ Just v

numericLastSetter :: ASetter a a b (Last Number) -> Updater a
numericLastSetter setter el str =
  case parseMayStartDot complexNumber str of
    Nothing -> el
    Just v -> el & setter .~ Last (Just v)


numMaySetter :: ASetter a a b (Last Float) -> Updater a
numMaySetter setter el str =
  case parseMayStartDot num str of
    Nothing -> el
    Just v -> el & setter .~ Last (Just v)

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

parserLastSetter :: ASetter a a b (Last e) -> Parser e -> Updater a
parserLastSetter setter parser el str =
  case parse parser str of
    Nothing -> el
    Just v -> el & setter .~ Last (Just v)

class XMLUpdatable a where
  attributes :: [(String, Updater a)]
  defaultSvg :: a

type CssUpdater =
    DrawAttributes -> [[CssElement]] -> DrawAttributes

cssUniqueNumber :: ASetter DrawAttributes DrawAttributes
                   a (Last Number)
                -> CssUpdater
cssUniqueNumber setter attr ((CssNumber n:_):_) = attr & setter .~ Last (Just n)
cssUniqueNumber _ attr _ = attr

cssUniqueFloat :: ASetter DrawAttributes DrawAttributes a Float
               -> CssUpdater
cssUniqueFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter .~ n
cssUniqueFloat _ attr _ = attr

cssUniqueMayFloat :: ASetter DrawAttributes DrawAttributes a (Last Float)
               -> CssUpdater
cssUniqueMayFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter .~ Last (Just n)
cssUniqueMayFloat _ attr _ = attr

cssIdentParser :: ASetter DrawAttributes DrawAttributes a b
               -> Parser b
               -> CssUpdater
cssIdentParser setter parser attr ((CssIdent i:_):_) =
  case parseOnly parser i of
    Left _ -> attr
    Right v -> attr & setter .~ v
cssIdentParser _ _ attr _ = attr

cssIdentStringParser :: ASetter DrawAttributes DrawAttributes a b
                     -> (String -> b) -> CssUpdater
cssIdentStringParser setter f attr ((CssIdent i:_):_) =
    attr & setter .~ f (T.unpack i)
cssIdentStringParser _ _ attr _ = attr

cssUniqueTexture :: ASetter DrawAttributes DrawAttributes
                    a (Last Texture)
                 -> CssUpdater
cssUniqueTexture setter attr ((CssIdent "none":_):_) =
    attr & setter .~ Last (Just FillNone)
cssUniqueTexture setter attr ((CssColor c:_):_) =
    attr & setter .~ Last (Just $ ColorRef c)
cssUniqueTexture _ attr _ = attr

cssMayStringSetter :: ASetter DrawAttributes DrawAttributes a (Maybe String)
                   -> CssUpdater
cssMayStringSetter setter attr ((CssIdent i:_):_) =
    attr & setter .~ Just (T.unpack i)
cssMayStringSetter setter attr ((CssString i:_):_) =
    attr & setter .~ Just (T.unpack i)
cssMayStringSetter _ attr _ = attr

cssLastStringSetter :: ASetter DrawAttributes DrawAttributes a (Last String)
                   -> CssUpdater
cssLastStringSetter setter attr ((CssIdent i:_):_) =
    attr & setter .~ Last (Just $ T.unpack i)
cssLastStringSetter setter attr ((CssString i:_):_) =
    attr & setter .~ Last (Just $ T.unpack i)
cssLastStringSetter _ attr _ = attr

cssDashArray :: ASetter DrawAttributes DrawAttributes a (Last [Number])
             -> CssUpdater
cssDashArray setter attr (lst:_) =
  case [n | CssNumber n <- lst ] of
    [] -> attr
    v -> attr & setter .~ Last (Just v)
cssDashArray _ attr _ = attr

drawAttributesList :: [(String, Updater DrawAttributes, CssUpdater)]
drawAttributesList =
    [("stroke-width", numericLastSetter strokeWidth, cssUniqueNumber strokeWidth)
    ,("stroke", parserSetter strokeColor (Last <$> textureParser),
        cssUniqueTexture strokeColor)
    ,("stroke-linecap",
        \e s -> e & strokeLineCap .~ parseCap s,
        cssIdentStringParser strokeLineCap parseCap)
    ,("stroke-linejoin", 
        \e s -> e & strokeLineJoin .~ parseLineJoin s,
        cssIdentStringParser strokeLineJoin parseLineJoin)
    ,("stroke-miterlimit", numMaySetter strokeMiterLimit,
        cssUniqueMayFloat strokeMiterLimit)
    ,("fill", parserSetter fillColor (Last <$> textureParser),
        cssUniqueTexture fillColor)
     -- TODO: fix this incompletness
    ,("transform", parserMaySetter transform (many transformParser), const)
    ,("opacity", numSetter fillOpacity, cssUniqueFloat fillOpacity)
    ,("fill-opacity", numSetter fillOpacity, cssUniqueFloat fillOpacity)
    ,("stroke-opacity", numSetter strokeOpacity, cssUniqueFloat strokeOpacity)
    ,("font-size", numericLastSetter fontSize, cssUniqueNumber fontSize)
    ,("font-family", \e s -> e & fontFamily .~ Last (Just $ commaSeparate s),
        cssIdentStringParser fontFamily (\s -> Last (Just $ commaSeparate s)))
        
    ,("fill-rule", \e s -> e & fillRule .~ parseFillRule s,
        cssIdentStringParser fillRule parseFillRule)
    ,("class", \e s -> e & attrClass .~ Last (Just s), cssLastStringSetter attrClass)
    ,("id", \e s -> e & attrId .~ Just s, cssMayStringSetter attrId)
    ,("stroke-dashoffset",numericLastSetter strokeOffset,
        cssUniqueNumber strokeOffset)
    ,("stroke-dasharray", parserLastSetter strokeDashArray  dashArray,
        cssDashArray strokeDashArray)
    ,("text-anchor", \e s -> e & textAnchor .~ parseTextAnchor s,
        cssIdentStringParser textAnchor parseTextAnchor)
    ]
  where commaSeparate =
            fmap (T.unpack . T.strip) . T.split (',' ==) . T.pack

instance XMLUpdatable DrawAttributes where
  defaultSvg = mempty
  attributes =
    stylePrepend [(attrName, updater)
                    | (attrName, updater, _) <- drawAttributesList]
    where stylePrepend lst = ("style", styleUpdater) : lst

styleUpdater :: DrawAttributes -> String
             -> DrawAttributes 
styleUpdater attrs style = case parse styleString style of
    Nothing -> attrs
    Just decls -> foldl' applyer attrs decls
  where
    cssUpdaters = [(T.pack n, u) | (n, _, u) <- drawAttributesList]
    applyer value (CssDeclaration txt elems) =
        case lookup txt cssUpdaters of
          Nothing -> value
          Just f -> f value elems

instance XMLUpdatable Rectangle where
  defaultSvg = defaultRectangle
  attributes =
    [("width", numericSetter rectWidth)
    ,("height", numericSetter rectHeight)
    ,("x", numericSetter (rectUpperLeftCorner._1))
    ,("y", numericSetter (rectUpperLeftCorner._2))
    ,("rx", numericSetter (rectCornerRadius._1))
    ,("ry", numericSetter (rectCornerRadius._2))
    ]

instance XMLUpdatable Line where
  defaultSvg = defaultLine
  attributes =
    [("x1", numericSetter $ linePoint1._1)
    ,("y1", numericSetter $ linePoint1._2)
    ,("x2", numericSetter $ linePoint2._1)
    ,("y2", numericSetter $ linePoint2._2)
    ]

instance XMLUpdatable Ellipse where
  defaultSvg = defaultEllipse
  attributes =
    [("cx", numericSetter $ ellipseCenter._1)
    ,("cy", numericSetter $ ellipseCenter._2)
    ,("rx", numericSetter ellipseXRadius)
    ,("ry", numericSetter ellipseYRadius)
    ]

instance XMLUpdatable Circle where
  defaultSvg = defaultCircle
  attributes =
    [("cx", numericSetter $ circleCenter._1)
    ,("cy", numericSetter $ circleCenter._2)
    ,("r", numericSetter circleRadius)
    ]

instance XMLUpdatable Polygon where
  defaultSvg = defaultPolygon
  attributes =
    [("points", parserSetter polygonPoints pointData)]

instance XMLUpdatable PolyLine where
  defaultSvg = defaultPolyLine
  attributes = 
    [("points", parserSetter polyLinePoints pointData)]

instance XMLUpdatable PathPrim where
  defaultSvg = defaultPathPrim
  attributes =
    [("d", parserSetter pathDefinition (many1 command))]

instance XMLUpdatable LinearGradient where
  defaultSvg = defaultLinearGradient
  attributes = 
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

instance XMLUpdatable (Group a) where
  defaultSvg = defaultGroup
  attributes = []

instance XMLUpdatable (Symbol a) where
  defaultSvg = Symbol defaultSvg
  attributes =
        [("viewBox", parserMaySetter (groupOfSymbol . groupViewBox) viewBox)]


instance XMLUpdatable RadialGradient where
  defaultSvg = defaultRadialGradient
  attributes =
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

instance XMLUpdatable Use where
  defaultSvg = defaultUse
  attributes =
    [("x", numericSetter $ useBase._1)
    ,("y", numericSetter $ useBase._2)
    ,("width", numericMaySetter useWidth)
    ,("height", numericMaySetter useHeight)
    ,("href", \e s -> e & useName .~ dropSharp s)
    ]

dropSharp :: String -> String
dropSharp ('#':rest) = rest
dropSharp a = a

instance XMLUpdatable TextInfo where
  defaultSvg = defaultTextInfo
  attributes =
    [("x", parserSetter textInfoX dashArray)
    ,("y", parserSetter textInfoY dashArray)
    ,("dx", parserSetter textInfoDX dashArray)
    ,("dy", parserSetter textInfoDY dashArray)
    ,("rotate", parserSetter textInfoRotate numberList )
    ,("textLength", numericMaySetter textInfoLength)
    ]

parseTextPathMethod :: String -> TextPathMethod
parseTextPathMethod s = case s of
  "align" -> TextPathAlign
  "stretch" -> TextPathStretch
  _ -> _textPathMethod defaultTextPath

parseTextPathSpacing :: String -> TextPathSpacing
parseTextPathSpacing s = case s of
  "auto" -> TextPathSpacingAuto
  "exact" -> TextPathSpacingExact
  _ -> _textPathSpacing defaultTextPath

instance XMLUpdatable TextPath where
  defaultSvg = defaultTextPath
  attributes =
    [("startOffset", numericSetter textPathStartOffset)
    ,("method",
        \e s -> e & textPathMethod .~ parseTextPathMethod s)
    ,("spacing",
        \e s -> e & textPathSpacing .~ parseTextPathSpacing s)
    ,("href", \e s -> e & textPathName .~ dropSharp s)
    ]

instance XMLUpdatable Text where
  defaultSvg = defaultText
  attributes =
      [("lengthAdjust", \e s -> e & textAdjust .~ parseTextAdjust s)]


instance XMLUpdatable Pattern where
  defaultSvg = defaultPattern
  attributes =
    [("viewBox", \e s -> e & patternViewBox .~ parse viewBox s)
    ,("width", numericSetter patternWidth)
    ,("height", numericSetter patternHeight)
    ,("x", numericSetter (patternPos._1))
    ,("y", numericSetter (patternPos._2))
    ]


unparseText :: [X.Content] -> ([TextSpanContent], Maybe TextPath)
unparseText = extractResult . go True
  where
    extractResult (a, b, _) = (a, b)

    go startStrip [] = ([], Nothing, startStrip)
    go startStrip (X.CRef _:rest) = go startStrip rest
    go startStrip (X.Elem e@(nodeName -> "tspan"):rest) =
        (SpanSub spans : trest, mpath, retStrip)
      where
        (trest, mpath, retStrip) = go restStrip rest
        (sub, _, restStrip) = go startStrip $ X.elContent e
        spans = TextSpan (xmlUnparse e) (xmlUnparse e) sub

    go startStrip (X.Elem e@(nodeName -> "tref"):rest) = 
        case attributeFinder "href" e of
          Nothing -> go startStrip rest
          Just v -> (SpanTextRef v : trest, mpath, stripRet)
            where (trest, mpath, stripRet) = go startStrip rest

    go startStrip (X.Elem e@(nodeName -> "textPath"):rest) = 
        case attributeFinder "href" e of
          Nothing -> go startStrip rest
          Just v -> (tsub ++ trest, pure path, retStrp)
            where
              path = (xmlUnparse e) { _textPathName = dropSharp v }
              (trest, _, retStrp) = go restStrip rest
              (tsub, _, restStrip) = go startStrip $ X.elContent e

    go startStrip (X.Elem _:rest) = go startStrip rest
    go startStrip (X.Text t:rest)
      | T.length cleanText == 0 = go startStrip rest
      | otherwise =
        (SpanText cleanText : trest, mpath, stripRet)
       where
         (trest, mpath, stripRet) = go subShouldStrip rest

         subShouldStrip = T.pack " " `T.isSuffixOf` cleanText

         space = T.singleton ' '
         singulariseSpaces tt
            | space `T.isPrefixOf` tt = space
            | otherwise = tt

         stripStart | startStrip = T.stripStart
                    | otherwise = id

         cleanText = stripStart
                   . T.concat
                   . fmap singulariseSpaces
                   . T.groupBy (\a b -> (a /= ' ' && b /= ' ') || a == b)
                   . T.filter (\c -> c /= '\n' && c /= '\r')
                   . T.map (\c -> if c == '\t' then ' ' else c)
                   . T.pack
                   $ X.cdData t

gradientOffsetSetter :: GradientStop -> String -> GradientStop
gradientOffsetSetter el str = el & gradientOffset .~ val
  where
    val = case parseMayStartDot complexNumber str of
      Nothing -> 0
      Just (Num n) -> n
      Just (Percent n) -> n
      Just (Em n) -> n

instance XMLUpdatable GradientStop where
    defaultSvg = GradientStop 0 (PixelRGBA8 0 0 0 255)
    attributes =
        [("offset", gradientOffsetSetter)
        ,("stop-color", parserSetter gradientColor colorParser)]
            

data Symbols = Symbols
    { symbols :: !(M.Map String Element)
    , cssStyle   :: [CssRule]
    }

emptyState :: Symbols
emptyState = Symbols mempty mempty
 
parseGradientStops :: X.Element -> [GradientStop]
parseGradientStops = concatMap unStop . elChildren
  where
    unStop e@(nodeName -> "stop") = [xmlUnparse e]
    unStop _ = []

withId :: X.Element -> (X.Element -> Element)
       -> State Symbols Tree
withId el f = case attributeFinder "id" el of
  Nothing -> return None
  Just elemId -> do
      modify $ \s ->
        s { symbols = M.insert elemId (f el) $ symbols s }
      return None

unparseDefs :: X.Element -> State Symbols Tree
unparseDefs e@(nodeName -> "pattern") = do
  subElements <- mapM unparse $ elChildren e
  withId e . const . ElementPattern $ pat { _patternElements = subElements}
    where
      pat = xmlUnparse e
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

treeModify
    :: (forall a. (XMLUpdatable a, WithDrawAttributes a)
            => a -> a)
    -> Tree -> Tree
treeModify f v = case v of
  None -> None
  UseTree n t -> UseTree n t
  SymbolTree e -> SymbolTree $ f e
  GroupTree e -> GroupTree $ f e
  Path e -> Path $ f e
  CircleTree e -> CircleTree $ f e
  PolyLineTree e -> PolyLineTree $ f e
  PolygonTree e -> PolygonTree $ f e
  EllipseTree e -> EllipseTree $ f e
  LineTree e -> LineTree $ f e
  RectangleTree e -> RectangleTree $ f e
  TextArea a e -> TextArea a $ f e


unparse :: X.Element -> State Symbols Tree
unparse e@(nodeName -> "style") = do
  case parseOnly (many1 ruleSet) . T.pack $ strContent e of
    Left _ -> return ()
    Right rules ->
      modify $ \s -> s { cssStyle = cssStyle s ++ rules }
  return None
unparse e@(nodeName -> "defs") = do
    mapM_ unparseDefs $ elChildren e
    return None
unparse e@(nodeName -> "symbol") = do
  symbolChildren <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone symbolChildren
  pure . SymbolTree $ groupNode & groupChildren .~ realChildren
  where
    groupNode = _groupOfSymbol $ xmlUnparseWithDrawAttr e
    isNotNone None = False
    isNotNone _ = True

unparse e@(nodeName -> "g") = do
  children <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone children
  pure . GroupTree $ xmlUnparseWithDrawAttr e & groupChildren .~ realChildren
  where
    isNotNone None = False
    isNotNone _ = True

unparse e@(nodeName -> "text") = do
  pathWithGeometry <- pathGeomtryOf path
  pure . TextArea pathWithGeometry $ xmlUnparse e & textRoot .~ root
    where
      (textContent, path) = unparseText $ X.elContent e
      
      pathGeomtryOf Nothing = pure Nothing
      pathGeomtryOf (Just pathInfo) = do
        pathElem <- gets $ M.lookup (_textPathName pathInfo) . symbols
        case pathElem of
          Nothing -> pure Nothing
          Just (ElementLinearGradient _) -> pure Nothing
          Just (ElementRadialGradient _) -> pure Nothing
          Just (ElementPattern _) -> pure Nothing
          Just (ElementGeometry (Path p)) ->
              pure . Just $ pathInfo { _textPathData = _pathDefinition p }
          Just (ElementGeometry _) -> pure Nothing

      root = TextSpan
           { _spanInfo = xmlUnparse e
           , _spanDrawAttributes = xmlUnparse e
           , _spanContent = textContent
           }

unparse e@(nodeName -> "ellipse") =
  pure . EllipseTree $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "rect") = 
  pure . RectangleTree $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "polyline") =
  pure . PolyLineTree $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "polygon") =
  pure . PolygonTree $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "circle") =
  pure . CircleTree $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "line") =
  pure . LineTree $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "path") =
  pure . Path $ xmlUnparseWithDrawAttr e
unparse e@(nodeName -> "use") = do
  let useInfo = xmlUnparseWithDrawAttr e
  element <- gets $ M.lookup (_useName useInfo) . symbols
  case element of
    Nothing -> pure None
    Just (ElementLinearGradient _) -> pure None
    Just (ElementRadialGradient _) -> pure None
    Just (ElementPattern _) -> pure None
    Just (ElementGeometry g) -> pure $ UseTree useInfo g
unparse _ = pure None

cssDeclApplyer :: DrawAttributes -> CssDeclaration
               -> DrawAttributes 
cssDeclApplyer value (CssDeclaration txt elems) = 
   case lookup txt cssUpdaters of
     Nothing -> value
     Just f -> f value elems
  where
    cssUpdaters = [(T.pack n, u) | (n, _, u) <- drawAttributesList]

cssApply :: [CssRule] -> Tree -> Tree
cssApply rules = zipTree go where
  go [] = None
  go ([]:_) = None
  go context@((t:_):_) = t & drawAttr .~ attr'
   where
     matchingDeclarations =
         findMatchingDeclarations rules context
     attr = view drawAttr t
     attr' = foldl' cssDeclApplyer attr matchingDeclarations
   

unparseDocument :: X.Element -> Maybe Document
unparseDocument e@(nodeName -> "svg") = Just $ Document 
    { _viewBox =
        attributeFinder "viewBox" e >>= parse viewBox
    , _elements = cssApply (cssStyle named) <$> elements
    , _width = lengthFind "width"
    , _height = lengthFind "height"
    , _definitions = symbols named
    }
  where
    (elements, named) =
        runState (mapM unparse $ elChildren e) emptyState
    lengthFind n =
        attributeFinder n e >>= parse complexNumber
unparseDocument _ = Nothing   

