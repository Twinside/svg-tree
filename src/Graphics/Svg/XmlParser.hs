{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Svg.XmlParser( xmlOfDocument
                             , unparseDocument

                             , SvgAttributeLens( .. )
                             , drawAttributesList
                             ) where


#if !MIN_VERSION_base(4,6,0)
import Text.Read( reads )
#else
import Text.Read( readMaybe )
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( pure, (<$>), (<$), (<*>) )
import Data.Foldable( foldMap )
import Data.Monoid( mempty )
#endif

import Control.Applicative( (<|>), many )

import Control.Lens hiding( transform, children, elements, element )
import Control.Monad.State.Strict( State, runState, modify, gets )
import Data.Maybe( fromMaybe, catMaybes )
import Data.Monoid( Last( Last ), getLast )
import Data.List( foldl', intercalate )
import Text.XML.Light.Proc( findAttrBy, elChildren, strContent )
import qualified Text.XML.Light as X
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Map as M
import Data.Attoparsec.Text( Parser, string, parseOnly, many1 )
import Codec.Picture( PixelRGBA8( .. ) )
import Graphics.Svg.Types
import Graphics.Svg.PathParser
import Graphics.Svg.ColorParser
import Graphics.Svg.CssTypes( CssDeclaration( .. )
                            , CssElement( .. )
                            , CssRule
                            , tserialize
                            )
import Graphics.Svg.CssParser( complexNumber
                             , num
                             , ruleSet
                             , dashArray
                             , styleString
                             , numberList )

import Text.Printf( printf )

{-import Debug.Trace-}

#if !MIN_VERSION_base(4,6,0)
readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
  [] -> Nothing
  (x, _):_ -> Just x
#endif

nodeName :: X.Element -> String
nodeName = X.qName . X.elName

attributeFinder :: String -> X.Element -> Maybe String
attributeFinder str =
    findAttrBy (\a -> X.qName a == str)

-- | Helper class to help simplify parsing code
-- for various attributes.
class ParseableAttribute a where
  aparse :: String -> Maybe a
  aserialize :: a -> Maybe String

instance ParseableAttribute v => ParseableAttribute (Maybe v) where
  aparse = fmap Just . aparse
  aserialize = (>>= aserialize)

instance ParseableAttribute v => ParseableAttribute (Last v) where
  aparse = fmap Last . aparse
  aserialize = aserialize . getLast

instance ParseableAttribute String where
  aparse = Just
  aserialize = Just

instance ParseableAttribute Number where
  aparse = parseMayStartDot complexNumber
  aserialize = Just . serializeNumber

instance ParseableAttribute [Number] where
  aparse = parse dashArray
  aserialize = Just . serializeDashArray

instance ParseableAttribute PixelRGBA8 where
  aparse = parse colorParser
  aserialize = Just . colorSerializer

instance ParseableAttribute [PathCommand] where
  aparse = parse pathParser
  aserialize = Just . serializeCommands

instance ParseableAttribute GradientPathCommand where
  aparse = parse gradientCommand
  aserialize = Just . serializeGradientCommand

instance ParseableAttribute [RPoint] where
  aparse = parse pointData
  aserialize = Just . serializePoints

instance ParseableAttribute Double where
  aparse = parseMayStartDot num
  aserialize v = Just $ printf "%g" v

instance ParseableAttribute Texture where
  aparse = parse textureParser
  aserialize = Just . textureSerializer

instance ParseableAttribute [Transformation] where
  aparse = parse $ many transformParser
  aserialize = Just . serializeTransformations

instance ParseableAttribute Alignment where
  aparse s = Just $ case s of
    "none" -> AlignNone
    "xMinYMin" -> AlignxMinYMin
    "xMidYMin" -> AlignxMidYMin
    "xMaxYMin" -> AlignxMaxYMin
    "xMinYMid" -> AlignxMinYMid
    "xMidYMid" -> AlignxMidYMid
    "xMaxYMid" -> AlignxMaxYMid
    "xMinYMax" -> AlignxMinYMax
    "xMidYMax" -> AlignxMidYMax
    "xMaxYMax" -> AlignxMaxYMax
    _ -> _aspectRatioAlign defaultSvg

  aserialize v = Just $ case v of
    AlignNone -> "none"
    AlignxMinYMin -> "xMinYMin"
    AlignxMidYMin -> "xMidYMin"
    AlignxMaxYMin -> "xMaxYMin"
    AlignxMinYMid -> "xMinYMid"
    AlignxMidYMid -> "xMidYMid"
    AlignxMaxYMid -> "xMaxYMid"
    AlignxMinYMax -> "xMinYMax"
    AlignxMidYMax -> "xMidYMax"
    AlignxMaxYMax -> "xMaxYMax"

instance ParseableAttribute MeshGradientType where
  aparse s = Just $ case s of
    "bilinear" -> GradientBilinear
    "bicubic" -> GradientBicubic
    _ -> GradientBilinear

  aserialize v = Just $ case v of
    GradientBilinear -> "bilinear"
    GradientBicubic -> "bicubic"

instance ParseableAttribute MeetSlice where
  aparse s = case s of
    "meet" -> Just Meet
    "slice" -> Just Slice
    _ -> Nothing

  aserialize v = Just $ case v of
    Meet -> "meet"
    Slice -> "slice"

instance ParseableAttribute PreserveAspectRatio where
  aserialize v = Just $ defer <> align <> meetSlice where
    defer = if _aspectRatioDefer v then "defer " else ""
    align = fromMaybe "" . aserialize $ _aspectRatioAlign v
    meetSlice = fromMaybe "" $ aserialize =<< _aspectRatioMeetSlice v

  aparse s = case words s of
      [] -> Nothing
      [align] -> Just $ defaultSvg { _aspectRatioAlign = alignOf align }
      ["defer", align] ->
          Just $ defaultSvg
            { _aspectRatioDefer = True
            , _aspectRatioAlign = alignOf align
            }
      [align, meet] ->
          Just $ defaultSvg
            { _aspectRatioMeetSlice = aparse meet
            , _aspectRatioAlign = alignOf align
            }
      ["defer", align, meet] ->
          Just $ PreserveAspectRatio
              { _aspectRatioDefer = True
              , _aspectRatioAlign = alignOf align
              , _aspectRatioMeetSlice = aparse meet
              }
      _ -> Nothing
    where
      alignOf = fromMaybe (_aspectRatioAlign defaultSvg) . aparse

instance ParseableAttribute Cap where
  aparse s = case s of
    "butt" -> Just CapButt
    "round" -> Just CapRound
    "square" -> Just CapSquare
    _ -> Nothing

  aserialize c = Just $ case c of
    CapButt -> "butt"
    CapRound -> "round"
    CapSquare -> "square"

instance ParseableAttribute TextAnchor where
  aparse s = case s of
    "middle" -> Just TextAnchorMiddle
    "start" -> Just TextAnchorStart
    "end" -> Just TextAnchorEnd
    _ -> Nothing

  aserialize t = Just $ case t of
    TextAnchorMiddle -> "middle"
    TextAnchorStart -> "start"
    TextAnchorEnd -> "end"

instance ParseableAttribute ElementRef where
  aparse s = case parseOnly pa $ T.pack s of
     Left _ -> Nothing
     Right v -> Just v
    where
      pa = (RefNone <$ string "none")
        <|> (Ref <$> urlRef)

  aserialize c = Just $ case c of
    Ref r -> "url(#" <> r <> ")"
    RefNone -> "none"

instance ParseableAttribute LineJoin where
  aparse s = case s of
    "miter" -> Just JoinMiter
    "round" -> Just JoinRound
    "bevel" -> Just JoinBevel
    _ -> Nothing

  aserialize j = Just $ case j of
    JoinMiter -> "miter"
    JoinRound -> "round"
    JoinBevel -> "bevel"

instance ParseableAttribute CoordinateUnits where
  aparse s = case s of
    "userSpaceOnUse" -> Just CoordUserSpace
    "objectBoundingBox" -> Just CoordBoundingBox
    _ -> Just CoordBoundingBox

  aserialize uni = Just $ case uni of
    CoordUserSpace -> "userSpaceOnUse"
    CoordBoundingBox -> "objectBoundingBox"

instance ParseableAttribute Spread where
  aparse s = case s of
    "pad" -> Just SpreadPad
    "reflect" -> Just SpreadReflect
    "repeat" -> Just SpreadRepeat
    _ -> Nothing

  aserialize s = Just $ case s of
    SpreadPad -> "pad"
    SpreadReflect -> "reflect"
    SpreadRepeat -> "repeat"

instance ParseableAttribute FillRule where
  aparse s = case s of
    "nonzero" -> Just FillNonZero
    "evenodd" -> Just FillEvenOdd
    _ -> Nothing

  aserialize f = Just $ case f of
    FillNonZero -> "nonzero"
    FillEvenOdd -> "evenodd"

instance ParseableAttribute TextAdjust where
  aparse s = Just $ case s of
    "spacing" -> TextAdjustSpacing
    "spacingAndGlyphs" -> TextAdjustSpacingAndGlyphs
    _ -> TextAdjustSpacing

  aserialize a = Just $ case a of
    TextAdjustSpacing -> "spacing"
    TextAdjustSpacingAndGlyphs -> "spacingAndGlyphs"

instance ParseableAttribute MarkerUnit where
  aparse s = case s of
    "strokeWidth" -> Just MarkerUnitStrokeWidth
    "userSpaceOnUse" -> Just MarkerUnitUserSpaceOnUse
    _ -> Nothing

  aserialize u = Just $ case u of
    MarkerUnitStrokeWidth -> "strokeWidth"
    MarkerUnitUserSpaceOnUse -> "userSpaceOnUse"

instance ParseableAttribute Overflow where
  aparse s = case s of
    "visible" -> Just OverflowVisible
    "hidden" -> Just OverflowHidden
    _ -> Nothing

  aserialize u = Just $ case u of
    OverflowVisible -> "visible"
    OverflowHidden -> "hidden"

instance ParseableAttribute MarkerOrientation where
  aparse s = case (s, readMaybe s) of
    ("auto", _) -> Just OrientationAuto
    (_, Just f) -> Just $ OrientationAngle f
    _ -> Nothing

  aserialize s = Just $ case s of
    OrientationAuto -> "auto"
    OrientationAngle f -> show f

instance ParseableAttribute (Double, Double, Double, Double) where
  aparse = parse viewBoxParser
  aserialize = Just . serializeViewBox

instance ParseableAttribute TextPathMethod where
  aparse s = case s of
    "align" -> Just TextPathAlign
    "stretch" -> Just TextPathStretch
    _ -> Nothing
  aserialize m = Just $ case m of
    TextPathAlign -> "align"
    TextPathStretch -> "stretch"

instance ParseableAttribute TextPathSpacing where
  aparse s = case s of
    "auto" -> Just TextPathSpacingAuto
    "exact" -> Just TextPathSpacingExact
    _ -> Nothing

  aserialize s = Just $ case s of
    TextPathSpacingAuto -> "auto"
    TextPathSpacingExact -> "exact"

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

  serializeTreeNode :: treeNode -> Maybe X.Element

setChildren :: X.Element -> [X.Content] -> X.Element
setChildren xNode children = xNode { X.elContent = children }

updateWithAccessor :: XMLUpdatable b => (a -> [b]) -> a -> Maybe X.Element -> Maybe X.Element
updateWithAccessor        _    _ Nothing = Nothing
updateWithAccessor accessor node (Just xNode) =
    Just . setChildren xNode . fmap  X.Elem . catMaybes $ serializeTreeNode <$> accessor node

genericSerializeNode :: (XMLUpdatable treeNode) => treeNode -> Maybe X.Element
genericSerializeNode node =
    Just . X.unode (xmlTagName node) $ concatMap generateAttribute attributes
  where
    generateAttribute attr = case _attributeSerializer attr node of
      Nothing -> []
      Just str -> return X.Attr
        { X.attrKey = xName $ _attributeName attr
        , X.attrVal = str
        }
        where
         xName "href" =
            X.QName { X.qName = "href"
                    , X.qURI = Nothing
                    , X.qPrefix = Just "xlink" }
         xName h = X.unqual h


mergeAttributes :: X.Element -> X.Element -> X.Element
mergeAttributes thisXml otherXml =
    thisXml { X.elAttribs = X.elAttribs otherXml ++ X.elAttribs thisXml }

genericSerializeWithDrawAttr :: (XMLUpdatable treeNode, WithDrawAttributes treeNode)
                             => treeNode -> Maybe X.Element
genericSerializeWithDrawAttr node = mergeAttributes <$> thisXml <*> drawAttrNode where
  thisXml = genericSerializeNode node
  drawAttrNode = genericSerializeNode $ node ^. drawAttr

type CssUpdater a =
    a -> [[CssElement]] -> a

opacitySetter :: String -> Lens' a (Maybe Float) -> SvgAttributeLens a
opacitySetter attribute elLens =
    SvgAttributeLens attribute updater serializer
  where
    serializer a = printf "%g" <$> a ^. elLens
    updater el str = case parseMayStartDot num str of
        Nothing -> el
        Just v -> el & elLens .~ Just (realToFrac v)

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

parseIn :: (Eq a, WithDefaultSvg s, ParseableAttribute a)
        => String -> Lens' s a -> SvgAttributeLens s
parseIn attribute elLens =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case aparse str of
        Nothing -> el
        Just v -> el & elLens .~ v

    serializer a
      | v /= defaultVal = aserialize v
      | otherwise = Nothing
      where
        v = a ^. elLens
        defaultVal = defaultSvg ^. elLens

parserLastSetter :: String -> Lens' a (Last e) -> (String -> Maybe e) -> Serializer e
                 -> SvgAttributeLens a
parserLastSetter attribute elLens parser serialize =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case parser str of
        Nothing -> el
        Just v -> el & elLens .~ Last (Just v)

    serializer a = getLast (a ^. elLens) >>= serialize

classSetter :: SvgAttributeLens DrawAttributes
classSetter = SvgAttributeLens "class" updater serializer
  where
    updater el str =
      el & attrClass .~ (T.split (== ' ') $ T.pack str)

    serializer a = case a ^. attrClass of
      [] -> Nothing
      lst -> Just . T.unpack $ T.intercalate " " lst

cssUniqueNumber :: ASetter el el
                   a (Last Number)
                -> CssUpdater el
cssUniqueNumber setter attr ((CssNumber n:_):_) =
    attr & setter .~ Last (Just n)
cssUniqueNumber _ attr _ = attr

cssUniqueFloat :: (Fractional n)
               => ASetter el el a (Maybe n)
               -> CssUpdater el
cssUniqueFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter .~ Just (realToFrac n)
cssUniqueFloat _ attr _ = attr

cssUniqueMayFloat :: ASetter el el a (Last Double)
               -> CssUpdater el
cssUniqueMayFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter .~ Last (Just n)
cssUniqueMayFloat _ attr _ = attr

cssIdentAttr :: ParseableAttribute a => Lens' el a -> CssUpdater el
cssIdentAttr setter attr ((CssIdent i:_):_) = case aparse $ T.unpack i of
    Nothing -> attr
    Just v -> attr & setter .~ v
cssIdentAttr _ attr _ = attr

fontFamilyParser :: CssUpdater DrawAttributes
fontFamilyParser attr (lst:_) = attr & fontFamily .~ fontNames
  where
    fontNames = Last . Just $ T.unpack <$> extractString lst

    extractString [] = []
    extractString (CssIdent n:rest) = n : extractString rest
    extractString (CssString n:rest) = n : extractString rest
    extractString (_:rest) = extractString rest
fontFamilyParser attr _ = attr


cssUniqueTexture :: ASetter el el
                    a (Last Texture)
                 -> CssUpdater el
cssUniqueTexture setter attr css = case css of
  ((CssIdent "none":_):_) -> attr & setter .~ Last (Just FillNone)
  ((CssIdent "currentColor":_):_) -> attr & setter .~ Last (Just FillCurrent)
  ((CssColor c:_):_) -> attr & setter .~ Last (Just $ ColorRef c)
  ((CssFunction "url" [CssReference c]:_):_) ->
        attr & setter .~ Last (Just . TextureRef $ T.unpack c)
  _ -> attr

cssUniqueColor :: ASetter el el a PixelRGBA8 -> CssUpdater el
cssUniqueColor setter attr css = case css of
  ((CssColor c:_):_) -> attr & setter .~ c
  _ -> attr

cssElementRefSetter :: Lens' el (Last ElementRef) -> CssUpdater el
cssElementRefSetter setter attr ((CssFunction "url" [CssReference c]:_):_) =
    attr & setter .~ Last (Just . Ref $ T.unpack c)
cssElementRefSetter setter attr ((CssIdent "none":_):_) =
    attr & setter .~ Last (Just RefNone)
cssElementRefSetter _ attr _ = attr

cssMayStringSetter :: ASetter el el a (Maybe String) -> CssUpdater el
cssMayStringSetter setter attr ((CssIdent i:_):_) =
    attr & setter .~ Just (T.unpack i)
cssMayStringSetter setter attr ((CssString i:_):_) =
    attr & setter .~ Just (T.unpack i)
cssMayStringSetter _ attr _ = attr

cssNullSetter :: CssUpdater a
cssNullSetter attr _ = attr

cssDashArray :: ASetter el el a (Last [Number]) -> CssUpdater el
cssDashArray setter attr (lst:_) =
  case [n | CssNumber n <- lst ] of
    [] -> attr
    v -> attr & setter .~ Last (Just v)
cssDashArray _ attr _ = attr


drawAttributesList :: [(SvgAttributeLens DrawAttributes, CssUpdater DrawAttributes)]
drawAttributesList =
  [("stroke-width" `parseIn` strokeWidth, cssUniqueNumber strokeWidth)
  ,("stroke" `parseIn` strokeColor, cssUniqueTexture strokeColor)
  ,("fill" `parseIn` fillColor, cssUniqueTexture fillColor)
  ,("stroke-linecap" `parseIn` strokeLineCap, cssIdentAttr strokeLineCap)
  ,("stroke-linejoin" `parseIn` strokeLineJoin, cssIdentAttr strokeLineJoin)
  ,("stroke-miterlimit" `parseIn` strokeMiterLimit,
       cssUniqueMayFloat strokeMiterLimit)

  ,("transform" `parseIn` transform, const)
  ,(opacitySetter "opacity" groupOpacity, cssUniqueFloat groupOpacity)
  ,(opacitySetter "fill-opacity" fillOpacity, cssUniqueFloat fillOpacity)
  ,(opacitySetter "stroke-opacity" strokeOpacity, cssUniqueFloat strokeOpacity)
  ,("font-size" `parseIn` fontSize, cssUniqueNumber fontSize)
  ,(parserLastSetter "font-family" fontFamily (Just . commaSeparate)
      (Just . intercalate ", "), fontFamilyParser)

  ,("fill-rule" `parseIn` fillRule, cssIdentAttr fillRule)
  ,("clip-rule" `parseIn` clipRule, cssIdentAttr clipRule)
  ,("mask" `parseIn` maskRef, cssElementRefSetter maskRef)
  ,(classSetter, cssNullSetter) -- can't set class in CSS
  ,("id" `parseIn` attrId, cssMayStringSetter attrId)
  ,("stroke-dashoffset" `parseIn` strokeOffset,
      cssUniqueNumber strokeOffset)
  ,("stroke-dasharray" `parseIn` strokeDashArray, cssDashArray strokeDashArray)
  ,("text-anchor" `parseIn` textAnchor, cssIdentAttr textAnchor)
  ,("clip-path" `parseIn` clipPathRef, cssElementRefSetter clipPathRef)
  ,("marker-end" `parseIn` markerEnd, cssElementRefSetter markerEnd)
  ,("marker-start" `parseIn` markerStart, cssElementRefSetter markerStart)
  ,("marker-mid" `parseIn` markerMid, cssElementRefSetter markerMid)
  ]
  where
    commaSeparate =
        fmap (T.unpack . T.strip) . T.split (',' ==) . T.pack

serializeDashArray :: [Number] -> String
serializeDashArray =
   intercalate ", " . fmap serializeNumber

instance XMLUpdatable DrawAttributes where
  xmlTagName _ = "DRAWATTRIBUTES"
  attributes =
      styleAttribute drawAttributesList : fmap fst drawAttributesList
  serializeTreeNode = genericSerializeNode

styleAttribute :: [(SvgAttributeLens a, CssUpdater a)] -> SvgAttributeLens a
styleAttribute styleAttrs = SvgAttributeLens
  { _attributeName       = "style"
  , _attributeUpdater    = updater
  , _attributeSerializer = const Nothing
  }
  where
    updater attrs style = case parse styleString style of
        Nothing -> attrs
        Just decls -> foldl' applyer attrs decls

    cssUpdaters = [(T.pack $ _attributeName n, u) | (n, u) <- styleAttrs]
    applyer value (CssDeclaration txt elems) =
        case lookup txt cssUpdaters of
          Nothing -> value
          Just f -> f value elems

instance XMLUpdatable Rectangle where
  xmlTagName _ = "rect"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["width" `parseIn` rectWidth
    ,"height" `parseIn` rectHeight
    ,"x" `parseIn` (rectUpperLeftCorner._1)
    ,"y" `parseIn` (rectUpperLeftCorner._2)
    ,"rx" `parseIn` (rectCornerRadius._1)
    ,"ry" `parseIn` (rectCornerRadius._2)
    ]

instance XMLUpdatable Image where
  xmlTagName _ = "image"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["width" `parseIn` imageWidth
    ,"height" `parseIn` imageHeight
    ,"x" `parseIn` (imageCornerUpperLeft._1)
    ,"y" `parseIn` (imageCornerUpperLeft._2)
    ,parserSetter "href" imageHref (Just . dropSharp) Just
    ,"preserveAspectRatio" `parseIn` imageAspectRatio
    ]

instance XMLUpdatable Line where
  xmlTagName _ = "line"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["x1" `parseIn` (linePoint1._1)
    ,"y1" `parseIn` (linePoint1._2)
    ,"x2" `parseIn` (linePoint2._1)
    ,"y2" `parseIn` (linePoint2._2)
    ]

instance XMLUpdatable Ellipse where
  xmlTagName _ = "ellipse"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["cx" `parseIn` (ellipseCenter._1)
    ,"cy" `parseIn` (ellipseCenter._2)
    ,"rx" `parseIn` ellipseXRadius
    ,"ry" `parseIn` ellipseYRadius
    ]

instance XMLUpdatable Circle where
  xmlTagName _ = "circle"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["cx" `parseIn` (circleCenter._1)
    ,"cy" `parseIn` (circleCenter._2)
    ,"r" `parseIn` circleRadius
    ]

instance XMLUpdatable Mask where
  xmlTagName _ = "mask"
  serializeTreeNode node =
      updateWithAccessor _maskContent node $
          genericSerializeWithDrawAttr node

  attributes =
    ["x" `parseIn` (maskPosition._1)
    ,"y" `parseIn` (maskPosition._2)
    ,"width" `parseIn` maskWidth
    ,"height" `parseIn` maskHeight
    ,"maskContentUnits" `parseIn` maskContentUnits
    ,"maskUnits" `parseIn` maskUnits
    ]

instance XMLUpdatable ClipPath where
  xmlTagName _ = "clipPath"
  serializeTreeNode node =
      updateWithAccessor _clipPathContent node $
          genericSerializeWithDrawAttr node
  attributes =
    ["clipPathUnits" `parseIn` clipPathUnits]

instance XMLUpdatable Polygon where
  xmlTagName _ = "polygon"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes = ["points" `parseIn` polygonPoints]

instance XMLUpdatable PolyLine where
  xmlTagName _ =  "polyline"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes = ["points" `parseIn` polyLinePoints]

instance XMLUpdatable Path where
  xmlTagName _ =  "path"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes = ["d" `parseIn` pathDefinition]

instance XMLUpdatable MeshGradientPatch where
  xmlTagName _ = "meshpatch"
  attributes = []
  serializeTreeNode node =
     updateWithAccessor _meshGradientPatchStops node $ genericSerializeNode node

instance XMLUpdatable MeshGradientRow where
  xmlTagName _ = "meshrow"
  serializeTreeNode node =
     updateWithAccessor _meshGradientRowPatches node $ genericSerializeNode node
  attributes = []

instance XMLUpdatable MeshGradient where
  xmlTagName _ = "meshgradient"
  serializeTreeNode node =
     updateWithAccessor _meshGradientRows node $ genericSerializeWithDrawAttr node
  attributes =
    ["x" `parseIn` meshGradientX
    ,"y" `parseIn` meshGradientY
    ,"type" `parseIn` meshGradientType
    ,"gradientUnits" `parseIn` meshGradientUnits
    ,"gradientTransform" `parseIn` meshGradientTransform
    ]


instance XMLUpdatable LinearGradient where
  xmlTagName _ = "linearGradient"
  serializeTreeNode node =
     updateWithAccessor _linearGradientStops node $ genericSerializeNode node

  attributes =
    ["gradientTransform" `parseIn` linearGradientTransform
    ,"gradientUnits" `parseIn` linearGradientUnits
    ,"spreadMethod" `parseIn` linearGradientSpread
    ,"x1" `parseIn` (linearGradientStart._1)
    ,"y1" `parseIn` (linearGradientStart._2)
    ,"x2" `parseIn` (linearGradientStop._1)
    ,"y2" `parseIn` (linearGradientStop._2)
    ]

instance XMLUpdatable Tree where
  xmlTagName _ = "TREE"
  attributes = []
  serializeTreeNode e = case e of
    None -> Nothing
    UseTree u _ -> serializeTreeNode u
    GroupTree g -> serializeTreeNode g
    SymbolTree s -> serializeTreeNode s
    PathTree p -> serializeTreeNode p
    CircleTree c -> serializeTreeNode c
    PolyLineTree p -> serializeTreeNode p
    PolygonTree p -> serializeTreeNode p
    EllipseTree el -> serializeTreeNode el
    LineTree l -> serializeTreeNode l
    RectangleTree r -> serializeTreeNode r
    TextTree Nothing t -> serializeTreeNode t
    ImageTree i -> serializeTreeNode i
    MeshGradientTree m -> serializeTreeNode m
    TextTree (Just p) t -> do
       textNode <- serializeTreeNode t
       pathNode <- serializeTreeNode p
       let sub = [X.Elem . setChildren pathNode $ X.elContent textNode]
       return $ setChildren textNode sub


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
     ["viewBox" `parseIn` (groupOfSymbol . groupViewBox)
     ,"preserveAspectRatio" `parseIn` (groupOfSymbol . groupAspectRatio)
     ]


instance XMLUpdatable RadialGradient where
  xmlTagName _ = "radialGradient"
  serializeTreeNode node =
     updateWithAccessor _radialGradientStops node $ genericSerializeNode node
  attributes =
    ["gradientTransform" `parseIn` radialGradientTransform
    ,"gradientUnits" `parseIn` radialGradientUnits
    ,"spreadMethod" `parseIn` radialGradientSpread
    ,"cx" `parseIn` (radialGradientCenter._1)
    ,"cy" `parseIn` (radialGradientCenter._2)
    ,"r"  `parseIn` radialGradientRadius
    ,"fx" `parseIn` radialGradientFocusX
    ,"fy" `parseIn` radialGradientFocusY
    ]

instance XMLUpdatable Use where
  xmlTagName _ = "use"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["x" `parseIn` (useBase._1)
    ,"y" `parseIn` (useBase._2)
    ,"width" `parseIn` useWidth
    ,"height" `parseIn` useHeight
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
    ,"textLength" `parseIn` textInfoLength
    ]
    where
      dashNotEmpty [] = Nothing
      dashNotEmpty lst = Just $ serializeDashArray lst

      rotateNotEmpty [] = Nothing
      rotateNotEmpty lst =
          Just . unwords $ printf "%g" <$> lst


instance XMLUpdatable TextPath where
  xmlTagName _ =  "textPath"
  serializeTreeNode = genericSerializeNode
  attributes =
    ["startOffset" `parseIn` textPathStartOffset
    ,"method" `parseIn` textPathMethod
    ,"spacing" `parseIn` textPathSpacing
    ,parserSetter "href" textPathName (Just . dropSharp) (Just . ('#':))
    ]

instance XMLUpdatable Text where
  xmlTagName _ = "text"
  serializeTreeNode = serializeText
  attributes = ["lengthAdjust" `parseIn` textAdjust]


instance XMLUpdatable Pattern where
  xmlTagName _ = "pattern"
  serializeTreeNode node =
     updateWithAccessor _patternElements node $ genericSerializeWithDrawAttr node
  attributes =
    ["viewBox" `parseIn` patternViewBox
    ,"patternUnits" `parseIn` patternUnit
    ,"width" `parseIn` patternWidth
    ,"height" `parseIn` patternHeight
    ,"x" `parseIn` (patternPos._1)
    ,"y" `parseIn` (patternPos._2)
    ,"preserveAspectRatio" `parseIn` patternAspectRatio
    ,parserSetter "href" patternHref (Just . dropSharp) (Just . ('#':))
    ,"patternTransform" `parseIn` patternTransform
    ]

instance XMLUpdatable Marker where
  xmlTagName _ = "marker"
  serializeTreeNode node =
     updateWithAccessor _markerElements node $ genericSerializeWithDrawAttr node
  attributes =
    ["refX" `parseIn` (markerRefPoint._1)
    ,"refY" `parseIn` (markerRefPoint._2)
    ,"markerWidth" `parseIn` markerWidth
    ,"markerHeight" `parseIn` markerHeight
    ,"patternUnits" `parseIn` markerUnits
    ,"orient" `parseIn` markerOrient
    ,"viewBox" `parseIn` markerViewBox
    ,"overflow" `parseIn` markerOverflow
    ,"preserveAspectRatio" `parseIn` markerAspectRatio
    ]

serializeText :: Text -> Maybe X.Element
serializeText topText = namedNode where
  namedNode = fmap (\x -> x { X.elName = X.unqual "text" }) topNode
  topNode = serializeSpan $ _textRoot topText

  serializeSpan tspan = case (info, drawInfo) of
    (Nothing, Nothing) -> Nothing
    (Just a, Nothing) -> Just $ setChildren a subContent
    (Nothing, Just b) -> Just $ setChildren b subContent
    (Just a, Just b) ->
        Just $ setChildren (mergeAttributes a b) subContent
    where
      info = genericSerializeNode $ _spanInfo tspan
      drawInfo = genericSerializeNode $ _spanDrawAttributes tspan
      subContent = catMaybes $ serializeContent <$> _spanContent tspan

  serializeContent (SpanText t) = Just . X.Text $ X.blank_cdata { X.cdData = T.unpack t }
  serializeContent (SpanTextRef _t) = Just . X.Text $ X.blank_cdata { X.cdData = "" }
  serializeContent (SpanSub sub) = X.Elem <$> serializeSpan sub

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
          Just v -> (tsub ++ trest, pure p, retStrp)
            where
              p = (xmlUnparse e) { _textPathName = dropSharp v }
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
        val = realToFrac $ case parseMayStartDot complexNumber str of
            Nothing -> 0
            Just (Num n) -> n
            Just (Px n) -> n
            Just (Percent n) -> n
            Just (Em n) -> n
            Just (Pc n) -> n
            Just (Mm n) -> n
            Just (Cm n) -> n
            Just (Point n) -> n
            Just (Inches n) -> n

instance XMLUpdatable GradientStop where
    xmlTagName _ = "stop"
    serializeTreeNode = genericSerializeNode
    attributes = styleAttribute cssAvailable : fmap fst cssAvailable ++ lst where
      cssAvailable :: [(SvgAttributeLens GradientStop, CssUpdater GradientStop)]
      cssAvailable =
          [(opacitySetter "stop-opacity" gradientOpacity, (cssUniqueFloat gradientOpacity))
          ,("stop-color" `parseIn` gradientColor, cssUniqueColor gradientColor)
          ]

      lst =
        [gradientOffsetSetter
        ,"path" `parseIn` gradientPath
        ]


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

parseMeshGradientPatches :: X.Element -> [MeshGradientPatch]
parseMeshGradientPatches = foldMap unparsePatch . elChildren where
  unparsePatch e@(nodeName -> "meshpatch") = [MeshGradientPatch $ parseGradientStops e]
  unparsePatch _ = []

parseMeshGradientRows :: X.Element -> [MeshGradientRow]
parseMeshGradientRows = foldMap unRows . elChildren where
  unRows e@(nodeName -> "meshrow") = [MeshGradientRow $ parseMeshGradientPatches e]
  unRows _ = []

withId :: X.Element -> (X.Element -> Element)
       -> State Symbols Tree
withId el f = case attributeFinder "id" el of
  Nothing -> return None
  Just elemId -> do
      modify $ \s ->
        s { symbols = M.insert elemId (f el) $ symbols s }
      return None

isDefTag :: String -> Bool
isDefTag n = n `elem` defList where
  defList =
    [ "pattern"
    , "marker"
    , "mask"
    , "clipPath"
    , "linearGradient"
    , "meshgradient"
    , "radialGradient"]

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
unparseDefs e@(nodeName -> "mask") = do
  children <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone children
      parsedMask = xmlUnparseWithDrawAttr e
  withId e . const . ElementMask $ parsedMask { _maskContent = realChildren }

unparseDefs e@(nodeName -> "clipPath") = do
  children <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone children
      parsedClip = xmlUnparseWithDrawAttr e
  withId e . const . ElementClipPath $ parsedClip { _clipPathContent = realChildren }

unparseDefs e@(nodeName -> "linearGradient") =
  withId e $ ElementLinearGradient . unparser
  where
    unparser ee =
      xmlUnparse ee & linearGradientStops .~ parseGradientStops ee

unparseDefs e@(nodeName -> "meshgradient") =
  withId e $ ElementMeshGradient . unparser
  where
    unparser ee =
      xmlUnparseWithDrawAttr ee & meshGradientRows .~ parseMeshGradientRows ee

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
      modify $ \s -> s { cssStyle = cssStyle s ++ rules }
  return None
unparse e@(nodeName -> "defs") = do
    mapM_ unparseDefs $ elChildren e
    return None
unparse e@(nodeName -> "symbol") = do
  symbolChildren <- mapM unparse $ elChildren e
  let realChildren = filter isNotNone symbolChildren
  pure . SymbolTree . Symbol $ groupNode & groupChildren .~ realChildren
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
  pathWithGeometry <- pathGeomtryOf tPath
  pure . TextTree pathWithGeometry $ xmlUnparse e & textRoot .~ root
    where
      (textContent, tPath) = unparseText $ X.elContent e

      pathGeomtryOf Nothing = pure Nothing
      pathGeomtryOf (Just pathInfo) = do
        pathElem <- gets $ M.lookup (_textPathName pathInfo) . symbols
        case pathElem of
          Nothing -> pure Nothing
          Just (ElementLinearGradient _) -> pure Nothing
          Just (ElementRadialGradient _) -> pure Nothing
          Just (ElementMeshGradient _) -> pure Nothing
          Just (ElementPattern _) -> pure Nothing
          Just (ElementMask _) -> pure Nothing
          Just (ElementClipPath _) -> pure Nothing
          Just (ElementMarker _) -> pure Nothing
          Just (ElementGeometry (PathTree p)) ->
              pure . Just $ pathInfo { _textPathData = _pathDefinition p }
          Just (ElementGeometry _) -> pure Nothing

      root = TextSpan
           { _spanInfo = xmlUnparse e
           , _spanDrawAttributes = xmlUnparse e
           , _spanContent = textContent
           }

unparse e = case nodeName e of
    "image" -> pure $ ImageTree parsed
    "ellipse" -> pure $ EllipseTree parsed
    "rect" -> pure $ RectangleTree parsed
    "polyline" -> pure $ PolyLineTree parsed
    "polygon" -> pure $ PolygonTree parsed
    "circle"-> pure $ CircleTree parsed
    "line"  -> pure $ LineTree parsed
    "path" -> pure $ PathTree parsed
    "meshgradient" ->
      pure $ MeshGradientTree $ parsed & meshGradientRows .~ parseMeshGradientRows e
    "use" -> pure $ UseTree parsed Nothing
    n | isDefTag n -> unparseDefs e
    _ -> pure None
  where
    parsed :: (XMLUpdatable a, WithDrawAttributes a) => a
    parsed = xmlUnparseWithDrawAttr e

unparseDocument :: FilePath -> X.Element -> Maybe Document
unparseDocument rootLocation e@(nodeName -> "svg") = Just Document
    { _viewBox =
        attributeFinder "viewBox" e >>= parse viewBoxParser
    , _elements = parsedElements
    , _width = lengthFind "width"
    , _height = lengthFind "height"
    , _definitions = symbols named
    , _description = ""
    , _styleRules = cssStyle named
    , _documentLocation = rootLocation
    }
  where
    (parsedElements, named) =
        runState (mapM unparse $ elChildren e) emptyState
    lengthFind n =
        attributeFinder n e >>= parse complexNumber
unparseDocument _ _ = Nothing

-- | Transform a SVG document to a XML node.
xmlOfDocument :: Document -> X.Element
xmlOfDocument doc =
    X.node (X.unqual "svg") (attrs, descTag ++ styleTag ++ defsTag ++ children)
  where
    attr name = X.Attr (X.unqual name)
    children = catMaybes [serializeTreeNode el | el <- _elements doc]

    defsTag | null defs = []
            | otherwise = [X.node (X.unqual "defs") defs]

    defs = catMaybes [elementRender k e | (k, e) <- M.assocs $ _definitions doc]

    elementRender k e = case e of
        ElementGeometry t -> serialize t
        ElementMarker m -> serialize m
        ElementMask m -> serialize m
        ElementClipPath c -> serialize c
        ElementPattern p -> serialize p
        ElementLinearGradient lg -> addId $ serializeTreeNode lg
        ElementRadialGradient rg -> addId $ serializeTreeNode rg
        ElementMeshGradient   mg -> addId $ serializeTreeNode mg
      where
        addId = fmap (X.add_attr $ attr "id" k)

        serialize :: (WithDrawAttributes e, XMLUpdatable e) => e -> Maybe X.Element
        serialize el = case el^.drawAttr.attrId of
          Nothing -> addId $ serializeTreeNode el
          Just _id ->
            let newNode = el & drawAttr.attrId .~ Just k in
            serializeTreeNode newNode

    docViewBox = case _viewBox doc of
        Nothing -> []
        Just b -> [attr "viewBox" $ serializeViewBox b]

    descTag = case _description doc of
        "" -> []
        txt -> [X.node (X.unqual "desc") txt]

    styleTag = case _styleRules doc of
        [] -> []
        rules -> [X.node (X.unqual "style")
                        ([attr "type" "text/css"], txt)]
          where txt = TL.unpack . TB.toLazyText $ foldMap tserialize rules

    attrs =
        docViewBox ++
        [attr "xmlns" "http://www.w3.org/2000/svg"
        ,attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
        ,attr "version" "1.1"] ++
        catMaybes [attr "width" . serializeNumber <$> _width doc
                  ,attr "height" . serializeNumber <$> _height doc
                  ]

