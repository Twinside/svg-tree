{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Svg.XmlParser( xmlOfDocument
                             , unparseDocument

                             , SvgAttributeLens( .. )
                             , drawAttributesList
                             ) where

import Control.Applicative( (<$>)
                          {-, (<*>)-}
                          , many
                          , pure )

import Control.Lens hiding( transform, children, elements, element )
import Control.Monad( join )
import Control.Monad.State.Strict( State, runState, modify, gets )
import Data.Maybe( catMaybes )
import Data.Monoid( mempty, Last( Last ), getLast )
import Data.List( foldl', intercalate )
import Text.XML.Light.Proc( findAttrBy, elChildren, strContent )
import Text.Read( readMaybe )
import qualified Text.XML.Light as X
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Attoparsec.Text( Parser, parseOnly, many1 )
import Graphics.Svg.Types
import Graphics.Svg.PathParser
import Graphics.Svg.ColorParser
import Graphics.Svg.CssTypes( CssDeclaration( .. )
                            , CssElement( .. )
                            , CssRule
                            )
import Graphics.Svg.CssParser( complexNumber
                             , num
                             , ruleSet
                             , dashArray
                             , styleString
                             , numberList )

import Text.Printf( printf )

{-import Debug.Trace-}

nodeName :: X.Element -> String
nodeName = X.qName . X.elName

attributeFinder :: String -> X.Element -> Maybe String
attributeFinder str =
    findAttrBy (\a -> X.qName a == str)

parseCap :: String -> Maybe Cap
parseCap "butt" = Just CapButt
parseCap "round" = Just CapRound
parseCap "square" = Just CapSquare
parseCap _ = Nothing

serializeCap :: Cap -> String
serializeCap c = case c of
  CapButt -> "butt"
  CapRound -> "round"
  CapSquare -> "square"

parseTextAnchor :: String -> Maybe TextAnchor
parseTextAnchor "middle" = Just TextAnchorMiddle
parseTextAnchor "start" = Just TextAnchorStart
parseTextAnchor "end" = Just TextAnchorEnd
parseTextAnchor _ = Nothing

serializeTextAnchor :: TextAnchor -> String
serializeTextAnchor t = case t of
  TextAnchorMiddle -> "middle"
  TextAnchorStart -> "start"
  TextAnchorEnd -> "end"

parseLineJoin :: String -> Maybe LineJoin
parseLineJoin "miter" = Just JoinMiter
parseLineJoin "round" = Just JoinRound
parseLineJoin "bevel" = Just JoinBevel
parseLineJoin _ = Nothing

serializeLineJoin :: LineJoin -> String
serializeLineJoin j = case j of
    JoinMiter -> "miter"
    JoinRound -> "round"
    JoinBevel -> "bevel"

parseGradientUnit :: String -> GradientUnits
parseGradientUnit "userSpaceOnUse" = GradientUserSpace
parseGradientUnit "objectBoundingBox" = GradientBoundingBox
parseGradientUnit _ = GradientBoundingBox

serializeGradientUnit :: GradientUnits -> String
serializeGradientUnit uni = case uni of
    GradientUserSpace -> "userSpaceOnUse"
    GradientBoundingBox -> "objectBoundingBox"

parseGradientSpread :: String -> Spread
parseGradientSpread "pad" = SpreadPad
parseGradientSpread "reflect" = SpreadReflect
parseGradientSpread "repeat" = SpreadRepeat
parseGradientSpread _ = SpreadPad

serializeGradientSpread :: Spread -> String
serializeGradientSpread spread = case spread of
    SpreadPad -> "pad"
    SpreadReflect -> "reflect"
    SpreadRepeat -> "repeat"

parseFillRule :: String -> Maybe FillRule
parseFillRule "nonzero" = Just FillNonZero
parseFillRule "evenodd" = Just FillEvenOdd
parseFillRule _ = Nothing

serializeFillRule :: FillRule -> String
serializeFillRule FillNonZero = "nonzero"
serializeFillRule FillEvenOdd = "evenodd"

parseTextAdjust :: String -> TextAdjust
parseTextAdjust "spacing" = TextAdjustSpacing
parseTextAdjust "spacingAndGlyphs" = TextAdjustSpacingAndGlyphs
parseTextAdjust _ = TextAdjustSpacing

parsePatternUnit :: String -> Maybe PatternUnit
parsePatternUnit str = case str of
  "userSpaceOnUse" -> Just PatternUnitUserSpaceOnUse
  "objectBoundingBox" -> Just PatternUnitObjectBoundingBox
  _ -> Nothing

serializePatternUnit :: PatternUnit -> String
serializePatternUnit p = case p of
  PatternUnitUserSpaceOnUse -> "userSpaceOnUse"
  PatternUnitObjectBoundingBox -> "objectBoundingBox"

serializeTextAdjust :: TextAdjust -> String
serializeTextAdjust adj = case adj of
    TextAdjustSpacing -> "spacing"
    TextAdjustSpacingAndGlyphs -> "spacingAndGlyphs"

parseMarkerUnit :: String -> Maybe MarkerUnit
parseMarkerUnit s = case s of
  "strokeWidth" -> Just MarkerUnitStrokeWidth
  "userSpaceOnUse" -> Just MarkerUnitUserSpaceOnUse
  _ -> Nothing

serializeMarkerUnit :: MarkerUnit -> String
serializeMarkerUnit u = case u of
  MarkerUnitStrokeWidth -> "strokeWidth"
  MarkerUnitUserSpaceOnUse -> "userSpaceOnUse"

parseOrientation :: String -> Maybe MarkerOrientation
parseOrientation s = case (s, readMaybe s) of
    ("auto", _) -> Just OrientationAuto
    (_, Just f) -> Just $ OrientationAngle f
    _ -> Nothing

serializeOrientation :: MarkerOrientation -> String
serializeOrientation s = case s of
    OrientationAuto -> "auto"
    OrientationAngle f -> show f

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
    grab value updater =
        case attributeFinder (_attributeName updater) el of
          Nothing -> value
          Just v -> _attributeUpdater updater value v

xmlUnparse :: (XMLUpdatable a) => X.Element -> a
xmlUnparse = xmlUpdate defaultSvg

xmlUnparseWithDrawAttr
    :: (XMLUpdatable a, WithDrawAttributes a)
    => X.Element -> a
xmlUnparseWithDrawAttr e =
    xmlUnparse e & drawAttr .~ xmlUnparse e

data SvgAttributeLens t = SvgAttributeLens
  { _attributeName       :: String
  , _attributeUpdater    :: t -> String -> t
  , _attributeSerializer :: t -> Maybe String
  }

class (WithDefaultSvg treeNode) => XMLUpdatable treeNode where
  xmlTagName :: treeNode -> String
  attributes :: [SvgAttributeLens treeNode]

  serializeTreeNode :: treeNode -> X.Element

setChildren :: X.Element -> [X.Content] -> X.Element
setChildren xNode children = xNode { X.elContent = children }

updateWithAccessor :: XMLUpdatable b => (a -> [b]) -> a -> X.Element -> X.Element
updateWithAccessor accessor node xNode =
    setChildren xNode $ X.Elem . serializeTreeNode <$> accessor node

genericSerializeNode :: (XMLUpdatable treeNode) => treeNode -> X.Element
genericSerializeNode node =
    X.unode (xmlTagName node) $ concatMap generateAttribute attributes
  where
    generateAttribute attr = case _attributeSerializer attr node of
      Nothing -> []
      Just str -> return X.Attr
        { X.attrKey = xName $ _attributeName attr
        , X.attrVal = str
        }
        where
         xName "href" = 
            X.QName	{ X.qName = "href", X.qURI = Nothing, X.qPrefix = Just "xlink" }
         xName h = X.unqual h


mergeAttributes :: X.Element -> X.Element -> X.Element
mergeAttributes thisXml otherXml =
    thisXml { X.elAttribs = X.elAttribs otherXml ++ X.elAttribs thisXml }

genericSerializeWithDrawAttr :: (XMLUpdatable treeNode, WithDrawAttributes treeNode)
                             => treeNode -> X.Element
genericSerializeWithDrawAttr node = mergeAttributes thisXml drawAttrNode where
  thisXml = genericSerializeNode node
  drawAttrNode = genericSerializeNode $ node ^. drawAttr

type CssUpdater =
    DrawAttributes -> [[CssElement]] -> DrawAttributes

numericSetter :: String -> Lens' a Number -> SvgAttributeLens a
numericSetter attribute elLens = SvgAttributeLens attribute updater serializer
  where
    updater el str = case parseMayStartDot complexNumber str of
        Nothing -> el
        Just v -> el & elLens .~ v

    serializer a = Just . serializeNumber $ a ^. elLens

numericMaySetter :: String -> Lens' a (Maybe Number) -> SvgAttributeLens a
numericMaySetter attribute elLens = SvgAttributeLens attribute updater serializer
  where
    updater el str = case parseMayStartDot complexNumber str of
        Nothing -> el
        Just v -> el & elLens .~ Just v

    serializer a = serializeNumber <$> a ^. elLens


numericLastSetter :: String -> Lens' a (Last Number) -> SvgAttributeLens a
numericLastSetter attribute elLens = SvgAttributeLens attribute updater serializer
  where
    updater el str = case parseMayStartDot complexNumber str of
        Nothing -> el
        Just v -> el & elLens .~ Last (Just v)

    serializer a = serializeNumber <$> getLast (a ^. elLens)

numLastSetter :: String -> Lens' a (Last Float) -> SvgAttributeLens a
numLastSetter attribute elLens = SvgAttributeLens attribute updater serializer
  where
    updater el str = case parseMayStartDot num str of
        Nothing -> el
        Just v -> el & elLens .~ Last (Just v)

    serializer a = printf "%g" <$> getLast (a ^. elLens)

groupOpacitySetter :: SvgAttributeLens DrawAttributes
groupOpacitySetter = SvgAttributeLens "opacity" updater serializer
  where
    updater el str = case parseMayStartDot num str of
        Nothing -> el
        Just v -> (el & fillOpacity .~ Just v) & strokeOpacity .~ Just v

    serializer a = case (a ^. fillOpacity, a ^. strokeOpacity) of
      (Just v, Just b) | v == b -> Just $ printf "%g" v
      _ -> Nothing

opacitySetter :: String -> Lens' a (Maybe Float) -> Lens' a (Maybe Float)
              -> SvgAttributeLens a
opacitySetter attribute elLens otherLens =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case parseMayStartDot num str of
        Nothing -> el
        Just v -> el & elLens .~ Just v

    serializer a = case (a ^. elLens, a ^. otherLens) of
        (Just v, Just b) | v == b -> Nothing
        (Just v, _) -> Just $ printf "%g" v
        (Nothing, _) -> Nothing

type Serializer e = e -> Maybe String

parserSetter :: String -> Lens' a e -> (String -> Maybe e) -> Serializer e
             -> SvgAttributeLens a
parserSetter attribute elLens parser serialize =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case parser str of
        Nothing -> el
        Just v -> el & elLens .~ v

    serializer  a = serialize $ a ^. elLens

parserMaySetter :: String -> Lens' a (Maybe e) -> (String -> Maybe e) -> Serializer e
                -> SvgAttributeLens a
parserMaySetter attribute elLens parser serialize =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case parser str of
        Nothing -> el
        Just v -> el & elLens .~ Just v

    serializer a = a ^. elLens >>= serialize

parserLastSetter :: String -> Lens' a (Last e) -> (String -> Maybe e) -> Serializer e
                 -> SvgAttributeLens a
parserLastSetter attribute elLens parser serialize =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case parser str of
        Nothing -> el
        Just v -> el & elLens .~ Last (Just v)

    serializer a = getLast (a ^. elLens) >>= serialize 

cssUniqueNumber :: ASetter DrawAttributes DrawAttributes
                   a (Last Number)
                -> CssUpdater
cssUniqueNumber setter attr ((CssNumber n:_):_) = attr & setter .~ Last (Just n)
cssUniqueNumber _ attr _ = attr

cssUniqueFloat :: ASetter DrawAttributes DrawAttributes a (Maybe Float)
               -> CssUpdater
cssUniqueFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter .~ Just n
cssUniqueFloat _ attr _ = attr

cssUniqueMayFloat :: ASetter DrawAttributes DrawAttributes a (Last Float)
               -> CssUpdater
cssUniqueMayFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter .~ Last (Just n)
cssUniqueMayFloat _ attr _ = attr

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


drawAttributesList :: [(SvgAttributeLens DrawAttributes, CssUpdater)]
drawAttributesList =
    [(numericLastSetter "stroke-width" strokeWidth, cssUniqueNumber strokeWidth)
    ,(parserLastSetter "stroke" strokeColor
        (join . parse textureParser)
        (Just . textureSerializer),
        cssUniqueTexture strokeColor)

    ,(parserLastSetter "fill" fillColor
        (join . parse textureParser)
        (Just . textureSerializer),
        cssUniqueTexture fillColor)

    ,(parserLastSetter "stroke-linecap" strokeLineCap parseCap
        (Just . serializeCap),
        cssIdentStringParser strokeLineCap (Last . parseCap))

    ,(parserLastSetter "stroke-linejoin" strokeLineJoin parseLineJoin
        (Just . serializeLineJoin) ,
        cssIdentStringParser strokeLineJoin (Last . parseLineJoin))
    ,(numLastSetter "stroke-miterlimit" strokeMiterLimit,
         cssUniqueMayFloat strokeMiterLimit)

    ,(parserMaySetter "transform"  transform
         (parse $ many transformParser)
         (Just . serializeTransformations), const)
    ,(groupOpacitySetter, cssUniqueFloat fillOpacity)
    ,(opacitySetter "fill-opacity" fillOpacity strokeOpacity,
        cssUniqueFloat fillOpacity)
    ,(opacitySetter "stroke-opacity" strokeOpacity fillOpacity,
        cssUniqueFloat strokeOpacity)
    ,(numericLastSetter "font-size" fontSize, cssUniqueNumber fontSize)
    ,(parserLastSetter "font-family" fontFamily (Just . commaSeparate)
        (Just . intercalate ", "),
        cssIdentStringParser fontFamily (\s -> Last (Just $ commaSeparate s)))
        
    ,(parserLastSetter "fill-rule" fillRule parseFillRule
        (Just . serializeFillRule),
        cssIdentStringParser fillRule (Last . parseFillRule))
    ,(parserLastSetter "class" attrClass Just Just, cssLastStringSetter attrClass)
    ,(parserMaySetter "id" attrId Just Just, cssMayStringSetter attrId)
    ,(numericLastSetter "stroke-dashoffset" strokeOffset,
        cssUniqueNumber strokeOffset)
    ,(parserLastSetter "stroke-dasharray" strokeDashArray
        (parse dashArray)
        (Just . serializeDashArray),
        cssDashArray strokeDashArray)
    ,(parserLastSetter "text-anchor" textAnchor parseTextAnchor
        (Just . serializeTextAnchor),
        cssIdentStringParser textAnchor (Last . parseTextAnchor))
    ]
  where
    commaSeparate =
        fmap (T.unpack . T.strip) . T.split (',' ==) . T.pack

serializeDashArray :: [Number] -> String
serializeDashArray =
   intercalate ", " . fmap serializeNumber 

instance XMLUpdatable DrawAttributes where
  xmlTagName _ = "DRAWATTRIBUTES"
  attributes = styleAttribute : fmap fst drawAttributesList
  serializeTreeNode = genericSerializeNode

styleAttribute :: SvgAttributeLens DrawAttributes
styleAttribute = SvgAttributeLens
  { _attributeName       = "style"
  , _attributeUpdater    = updater
  , _attributeSerializer = const Nothing
  }
  where
    updater attrs style = case parse styleString style of
        Nothing -> attrs
        Just decls -> foldl' applyer attrs decls

    cssUpdaters = [(T.pack $ _attributeName n, u) | (n, u) <- drawAttributesList]
    applyer value (CssDeclaration txt elems) =
        case lookup txt cssUpdaters of
          Nothing -> value
          Just f -> f value elems

instance XMLUpdatable Rectangle where
  xmlTagName _ = "rect"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [numericSetter "width" rectWidth
    ,numericSetter "height" rectHeight
    ,numericSetter "x" (rectUpperLeftCorner._1)
    ,numericSetter "y" (rectUpperLeftCorner._2)
    ,numericSetter "rx" (rectCornerRadius._1)
    ,numericSetter "ry" (rectCornerRadius._2)
    ]

instance XMLUpdatable Line where
  xmlTagName _ = "line"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [numericSetter "x1" $ linePoint1._1
    ,numericSetter "y1" $ linePoint1._2
    ,numericSetter "x2" $ linePoint2._1
    ,numericSetter "y2" $ linePoint2._2
    ]

instance XMLUpdatable Ellipse where
  xmlTagName _ = "ellipse"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [numericSetter "cx" $ ellipseCenter._1
    ,numericSetter "cy" $ ellipseCenter._2
    ,numericSetter "rx" ellipseXRadius
    ,numericSetter "ry" ellipseYRadius
    ]

instance XMLUpdatable Circle where
  xmlTagName _ = "circle"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [numericSetter "cx" $ circleCenter._1
    ,numericSetter "cy" $ circleCenter._2
    ,numericSetter "r" circleRadius
    ]

instance XMLUpdatable Polygon where
  xmlTagName _ = "polygon"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [parserSetter "points" polygonPoints
        (parse pointData)
        (Just . serializePoints)]

instance XMLUpdatable PolyLine where
  xmlTagName _ =  "polyline"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes = 
    [parserSetter "points" polyLinePoints
        (parse pointData)
        (Just . serializePoints)]

instance XMLUpdatable PathPrim where
  xmlTagName _ =  "path"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [parserSetter "d" pathDefinition
        (parse $ many1 command)
        (Just . serializeCommands)]

instance XMLUpdatable LinearGradient where
  xmlTagName _ = "linearGradient"
  serializeTreeNode node =
     updateWithAccessor _linearGradientStops node $ genericSerializeNode node
        
  attributes = 
    [parserSetter "gradientTransform" linearGradientTransform
        (parse $ many transformParser)
        (Just . serializeTransformations)
    ,parserSetter "gradientUnits" linearGradientUnits
        (Just . parseGradientUnit)
        (Just . serializeGradientUnit)
    ,parserSetter "spreadMethod" linearGradientSpread
        (Just . parseGradientSpread)
        (Just . serializeGradientSpread)
    ,numericSetter "x1" $ linearGradientStart._1
    ,numericSetter "y1" $ linearGradientStart._2
    ,numericSetter "x2" $ linearGradientStop._1
    ,numericSetter "y2" $ linearGradientStop._2
    ]

instance XMLUpdatable Tree where
  xmlTagName _ = "TREE"
  attributes = []
  serializeTreeNode e = case e of
    None -> X.blank_element
    UseTree u _ -> serializeTreeNode u
    GroupTree g -> serializeTreeNode g
    SymbolTree s -> serializeTreeNode s
    Path p -> serializeTreeNode p
    CircleTree c -> serializeTreeNode c
    PolyLineTree p -> serializeTreeNode p
    PolygonTree p -> serializeTreeNode p
    EllipseTree el -> serializeTreeNode el
    LineTree l -> serializeTreeNode l
    RectangleTree r -> serializeTreeNode r
    TextArea Nothing t -> serializeTreeNode t
    TextArea (Just p) t ->
        setChildren textNode [X.Elem . setChildren pathNode $ X.elContent textNode]
      where
        textNode = serializeTreeNode t
        pathNode = serializeTreeNode p


isNotNone :: Tree -> Bool
isNotNone None = False
isNotNone _ = True

instance XMLUpdatable (Group Tree) where
  xmlTagName _ = "g"
  serializeTreeNode node =
     updateWithAccessor (filter isNotNone . _groupChildren) node $
        genericSerializeWithDrawAttr node
  attributes = []

instance XMLUpdatable (Symbol Tree) where
  xmlTagName _ = "symbol"
  serializeTreeNode node =
     updateWithAccessor (filter isNotNone . _groupChildren . _groupOfSymbol) node $
        genericSerializeWithDrawAttr node
  attributes =
        [parserMaySetter "viewBox" (groupOfSymbol . groupViewBox)
            (parse viewBox)
            (Just . serializeViewBox)
        ]


instance XMLUpdatable RadialGradient where
  xmlTagName _ = "radialGradient"
  serializeTreeNode node =
     updateWithAccessor _radialGradientStops node $ genericSerializeNode node
  attributes =
    [parserSetter "gradientTransform" radialGradientTransform
        (parse $ many transformParser)
        (Just . serializeTransformations)
    ,parserSetter "gradientUnits" radialGradientUnits
        (Just . parseGradientUnit)
        (Just . serializeGradientUnit)
    ,parserSetter "spreadMethod" radialGradientSpread
        (Just . parseGradientSpread)
        (Just . serializeGradientSpread)
    ,numericSetter "cx" $ radialGradientCenter._1
    ,numericSetter "cy" $ radialGradientCenter._2
    ,numericSetter "r" radialGradientRadius
    ,numericMaySetter "fx" radialGradientFocusX
    ,numericMaySetter "fy" radialGradientFocusY
    ]

instance XMLUpdatable Use where
  xmlTagName _ = "use"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [numericSetter "x" $ useBase._1
    ,numericSetter "y" $ useBase._2
    ,numericMaySetter "width" useWidth
    ,numericMaySetter "height" useHeight
    ,parserSetter "href" useName (Just . dropSharp) (Just . ('#':))
    ]

dropSharp :: String -> String
dropSharp ('#':rest) = rest
dropSharp a = a

instance XMLUpdatable TextInfo where
  xmlTagName _ = "tspan"
  serializeTreeNode = genericSerializeNode
  attributes =
    [parserSetter "x" textInfoX (parse dashArray) dashNotEmpty
    ,parserSetter "y" textInfoY (parse dashArray) dashNotEmpty
    ,parserSetter "dx" textInfoDX (parse dashArray) dashNotEmpty
    ,parserSetter "dy" textInfoDY (parse dashArray) dashNotEmpty
    ,parserSetter "rotate" textInfoRotate 
        (parse numberList)
        rotateNotEmpty
    ,numericMaySetter "textLength" textInfoLength
    ]
    where
      dashNotEmpty [] = Nothing
      dashNotEmpty lst = Just $ serializeDashArray lst

      rotateNotEmpty [] = Nothing
      rotateNotEmpty lst =
          Just . unwords $ printf "%g" <$> lst


parseTextPathMethod :: String -> TextPathMethod
parseTextPathMethod s = case s of
  "align" -> TextPathAlign
  "stretch" -> TextPathStretch
  _ -> _textPathMethod defaultSvg

serializeTextPathMethod :: TextPathMethod -> String
serializeTextPathMethod m = case m of
  TextPathAlign -> "align"
  TextPathStretch -> "stretch"

parseTextPathSpacing :: String -> TextPathSpacing
parseTextPathSpacing s = case s of
  "auto" -> TextPathSpacingAuto
  "exact" -> TextPathSpacingExact
  _ -> _textPathSpacing defaultSvg

serializeTextPathSpacing :: TextPathSpacing -> String
serializeTextPathSpacing s = case s of
  TextPathSpacingAuto -> "auto"
  TextPathSpacingExact -> "exact"

instance XMLUpdatable TextPath where
  xmlTagName _ =  "textPath"
  serializeTreeNode = genericSerializeNode
  attributes =
    [numericSetter "startOffset" textPathStartOffset
    ,parserSetter "method"textPathMethod
        (Just . parseTextPathMethod)
        (Just . serializeTextPathMethod)
    ,parserSetter "spacing" textPathSpacing
        (Just . parseTextPathSpacing)
        (Just . serializeTextPathSpacing)
    ,parserSetter "href" textPathName (Just . dropSharp) (Just . ('#':))
    ]

instance XMLUpdatable Text where
  xmlTagName _ = "text"
  serializeTreeNode = serializeText
  attributes =
      [parserSetter "lengthAdjust" textAdjust 
         (Just . parseTextAdjust)
         (Just . serializeTextAdjust)]


instance XMLUpdatable Pattern where
  xmlTagName _ = "pattern"
  serializeTreeNode node =
     updateWithAccessor _patternElements node $ genericSerializeNode node
  attributes =
    [parserMaySetter "viewBox" patternViewBox 
        (parse viewBox)
        (Just . serializeViewBox)
    ,parserSetter "patternUnits" patternUnit
        parsePatternUnit
        (Just . serializePatternUnit)
    ,numericSetter "width" patternWidth
    ,numericSetter "height" patternHeight
    ,numericSetter "x" (patternPos._1)
    ,numericSetter "y" (patternPos._2)
    ]

instance XMLUpdatable Marker where
  xmlTagName _ = "marker"
  serializeTreeNode node =
     updateWithAccessor _markerElements node $ genericSerializeNode node
  attributes =
    [numericSetter "refX" (markerRefPoint._1)
    ,numericSetter "refY" (markerRefPoint._2)
    ,numericSetter "markerWidth" markerWidth
    ,numericSetter "markerHeight" markerHeight
    ,parserMaySetter "patternUnits" markerUnits
        parseMarkerUnit
        (Just . serializeMarkerUnit)
    ,parserMaySetter "patternUnits" markerOrient
        parseOrientation
        (Just . serializeOrientation)
    ]

serializeText :: Text -> X.Element
serializeText topText = topNode { X.elName = X.unqual "text" } where
  topNode = serializeSpan $ _textRoot topText 
  
  serializeSpan tspan = setChildren (mergeAttributes info drawInfo) subContent
    where
      info = genericSerializeNode $ _spanInfo tspan
      drawInfo = genericSerializeNode $ _spanDrawAttributes tspan
      subContent = serializeContent <$> _spanContent tspan

  serializeContent (SpanText t) = X.Text $ X.blank_cdata { X.cdData = T.unpack t }
  serializeContent (SpanTextRef _t) = X.Text $ X.blank_cdata { X.cdData = "" }
  serializeContent (SpanSub sub) = X.Elem $ serializeSpan sub

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

gradientOffsetSetter :: SvgAttributeLens GradientStop
gradientOffsetSetter = SvgAttributeLens "offset" setter serialize
  where
    serialize a = Just $ printf "%d%%" percentage
      where percentage = floor . (100 *) $ a ^. gradientOffset :: Int

    setter el str = el & gradientOffset .~ val
      where 
        val = case parseMayStartDot complexNumber str of
            Nothing -> 0
            Just (Num n) -> n
            Just (Percent n) -> n
            Just (Em n) -> n

instance XMLUpdatable GradientStop where
    xmlTagName _ = "stop"
    serializeTreeNode = genericSerializeNode
    attributes =
        [gradientOffsetSetter
        ,parserSetter "stop-color" gradientColor
            (parse colorParser)
            (Just . colorSerializer)
        ]
            

data Symbols = Symbols
    { symbols :: !(M.Map String Element)
    , cssStyle   :: [CssRule]
    , cssText    :: String
    }

emptyState :: Symbols
emptyState = Symbols mempty mempty ""
 
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
unparseDefs e@(nodeName -> "marker") = do
  subElements <- mapM unparse $ elChildren e
  withId e . const . ElementMarker $ mark {_markerElements = subElements }
    where
      mark = xmlUnparseWithDrawAttr e
unparseDefs e@(nodeName -> "linearGradient") =
  withId e $ ElementLinearGradient . unparser
  where
    unparser ee =
      xmlUnparse ee & linearGradientStops .~ parseGradientStops ee
unparseDefs e@(nodeName -> "radialGradient") =
  withId e $ ElementRadialGradient . unparser
  where
    unparser ee =
      xmlUnparse ee & radialGradientStops .~ parseGradientStops ee
unparseDefs e = do
  el <- unparse e
  withId e (const $ ElementGeometry el)

unparse :: X.Element -> State Symbols Tree
unparse e@(nodeName -> "style") = do
  case parseOnly (many1 ruleSet) . T.pack $ strContent e of
    Left _ -> return ()
    Right rules ->
      modify $ \s -> s { cssStyle = cssStyle s ++ rules
                      , cssText = cssText s ++ "\n" ++ strContent e }
  return None
unparse e@(nodeName -> "defs") = do
    mapM_ unparseDefs $ elChildren e
    return None
unparse e@(nodeName -> "symbol") = do
  symbolChildren <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone symbolChildren
  pure . SymbolTree $ groupNode & groupChildren .~ realChildren
  where
    groupNode :: Group Tree
    groupNode = _groupOfSymbol $ xmlUnparseWithDrawAttr e

unparse e@(nodeName -> "g") = do
  children <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone children

      groupNode :: Group Tree
      groupNode = xmlUnparseWithDrawAttr e 

  pure $ GroupTree $ groupNode & groupChildren .~ realChildren

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
          Just (ElementMarker _) -> pure Nothing
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
    Just (ElementMarker _) -> pure None
    Just (ElementGeometry g) -> pure $ UseTree useInfo g
unparse _ = pure None

unparseDocument :: X.Element -> Maybe Document
unparseDocument e@(nodeName -> "svg") = Just Document 
    { _viewBox =
        attributeFinder "viewBox" e >>= parse viewBox
    , _elements = elements
    , _width = lengthFind "width"
    , _height = lengthFind "height"
    , _definitions = symbols named
    , _description = ""
    , _styleRules = cssStyle named
    , _styleText = cssText named
    }
  where
    (elements, named) =
        runState (mapM unparse $ elChildren e) emptyState
    lengthFind n =
        attributeFinder n e >>= parse complexNumber
unparseDocument _ = Nothing   

-- | Transform a SVG document to a XML node.
xmlOfDocument :: Document -> X.Element
xmlOfDocument doc =
    X.node (X.unqual "svg") (attrs, descTag ++ styleTag ++ defsTag ++ children)
  where
    attr name = X.Attr (X.unqual name)
    children = [serializeTreeNode el | el <- _elements doc, isNotNone el ]

    defsTag | null defs = []
            | otherwise = [X.node (X.unqual "defs") defs]

    defs = [elementRender k e | (k, e) <- M.assocs $ _definitions doc
                              , isElementNotNone e]

    isElementNotNone (ElementGeometry el) = isNotNone el
    isElementNotNone _ = True

    elementRender k e = case e of
        ElementGeometry t -> serializeTreeNode t
        ElementMarker m -> serializeTreeNode m
        ElementPattern p -> serializeTreeNode p
        ElementLinearGradient lg ->
            X.add_attr (attr "id" k) $ serializeTreeNode lg
        ElementRadialGradient rg ->
            X.add_attr (attr "id" k) $ serializeTreeNode rg

    docViewBox = case _viewBox doc of
        Nothing -> []
        Just b -> [attr "viewBox" $ serializeViewBox b]

    {-style [] = []-}
    {-style lst =-}

    descTag = case _description doc of
        "" -> []
        txt -> [X.node (X.unqual "desc") txt]

    styleTag = case _styleText doc of
        "" -> []
        txt -> [X.node (X.unqual "style")
                    ([attr "type" "text/css"], txt)]

    attrs =
        docViewBox ++
        [attr "xmlns" "http://www.w3.org/2000/svg"
        ,attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
        ,attr "version" "1.1"] ++
        catMaybes [attr "width" . serializeNumber <$> _width doc
                  ,attr "height" . serializeNumber <$> _height doc
                  ]

