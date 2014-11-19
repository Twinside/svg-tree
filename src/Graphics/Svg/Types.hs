{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.Svg.Types
    ( Coord
    , Origin( .. )
    , Point
    , SvgDocument( .. )
    , SvgPath( .. )
    , SvgCap( .. )
    , SvgLineJoin( .. )
    , SvgTree( .. )
    , SvgNumber( .. )
    , SvgTransformation( .. )
    , SvgPoint
    , SvgSpread( .. )
    , SvgTexture( .. )
    , SvgElement( .. )
    , SvgFillRule( .. )
    , SvgFontStyle( .. )
    , toSvgPoint
    , svgDocumentSize

    , GradientUnits( .. )

    , SvgDrawAttributes( .. )
    , HasSvgDrawAttributes( .. )

    , SvgGradientStop( .. )
    , HasSvgGradientStop( .. )

    , SvgLinearGradient( .. )
    , defaultLinearGradient
    , HasSvgLinearGradient( .. )

    , SvgRadialGradient( .. )
    , defaultRadialGradient
    , HasSvgRadialGradient( .. )

    , SvgRectangle( .. )
    , defaultRectangle
    , HasSvgRectangle( .. )

    , SvgLine( .. )
    , defaultLine
    , HasSvgLine( .. )

    , SvgPolygon( .. )
    , defaultPolygon
    , HasSvgPolygon( .. )

    , SvgPolyLine( .. )
    , defaultPolyLine
    , HasSvgPolyLine( .. )

    , SvgPathPrim( .. )
    , defaultPathPrim
    , HasSvgPathPrim( .. )

    , SvgGroup( .. )
    , defaultGroup
    , svgGroupDrawAttributes
    , svgGroupChildren
    , svgGroupViewBox

    , SvgSymbol( .. )
    , groupOfSymbol

    , SvgCircle( .. )
    , defaultCircle
    , HasSvgCircle( .. )

    , SvgEllipse( .. )
    , defaultEllipse
    , HasSvgEllipse( .. )

    , SvgUse( .. )
    , defaultSvgUse
    , HasSvgUse( .. )

    , SvgText( .. )
    , defaultSvgText
    , HasSvgText( .. )

    , SvgTextPath( .. )
    , defaultSvgTextPath 
    , HasSvgTextPath( .. )

    , SvgTextPathSpacing( .. )
    , SvgTextPathMethod( .. )
    , SvgTextAnchor( .. )

    , SvgTextSpanContent( .. )

    , SvgTextSpan( .. )
    , defaultSvgTextSpan
    , HasSvgTextSpan( .. )

    , SvgTextInfo( .. )
    , CharInfo( .. )
    , emptyCharInfo
    , infinitizeTextInfo
    , unconsTextInfo
    , textInfoRests
    , mapTextInfoLists
    , defaultSvgTextInfo
    , HasSvgTextInfo( .. )

    , SvgTextAdjust( .. )

    , WithSvgDrawAttributes( .. )
    , isPathArc
    , isPathWithArc
    , nameOfTree
    , zipSvgTree
    ) where

import Data.Function( on )
import Data.List( inits )
import qualified Data.Map as M
import Data.Monoid( Monoid( .. ), Last( .. ), (<>) )
import Data.Foldable( Foldable )
import qualified Data.Foldable as F
import qualified Data.Text as T
import Codec.Picture( PixelRGBA8( .. ) )
import Graphics.Rasterific.Transformations( Transformation )
import Graphics.Rasterific( Point ) 
import Control.Lens( Lens'
                   , lens
                   , makeClassy
                   , makeLenses
                   , (^.)
                   , (&)
                   , (.~)
                   )

type Coord = Float

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

data SvgNumber
    = SvgNum Coord
    | SvgEm Coord
    | SvgPercent Coord
    deriving (Eq, Show)

type SvgPoint = (SvgNumber, SvgNumber)

toSvgPoint :: SvgNumber -> SvgNumber -> SvgPoint
toSvgPoint = (,)

isPathArc :: SvgPath -> Bool
isPathArc (ElipticalArc _ _) = True
isPathArc _ = False

isPathWithArc :: Foldable f => f SvgPath -> Bool
isPathWithArc = F.any isPathArc


data SvgCap
    = SvgCapRound
    | SvgCapButt
    | SvgCapSquare
    deriving (Eq, Show)

data SvgLineJoin
    = SvgJoinMiter
    | SvgJoinBevel
    | SvgJoinRound
    deriving (Eq, Show)

data SvgTexture
    = ColorRef   PixelRGBA8
    | TextureRef String
    | FillNone
    deriving (Eq, Show)

data SvgFillRule
    = SvgFillEvenOdd
    | SvgFillNonZero
    deriving (Eq, Show)

data SvgTransformation
    = SvgTransformMatrix Transformation
    | SvgTranslate Float Float
    | SvgScale Float (Maybe Float)
    | SvgRotate Float (Maybe (Float, Float))
    | SvgSkewX Float
    | SvgSkewY Float
    | SvgTransformUnknown
    deriving (Eq, Show)

class WithSvgDrawAttributes a where
    drawAttr :: Lens' a SvgDrawAttributes

data SvgFontStyle
    = FontStyleNormal
    | FontStyleItalic
    | FontStyleOblique
    deriving (Eq, Show)

data SvgTextAnchor
  = SvgTextAnchorStart
  | SvgTextAnchorMiddle
  | SvgTextAnchorEnd
  deriving (Eq, Show)

data SvgDrawAttributes = SvgDrawAttributes
    { _strokeWidth      :: !(Last SvgNumber)
    , _strokeColor      :: !(Last SvgTexture)
    , _strokeOpacity    :: !Float
    , _strokeLineCap    :: !(Last SvgCap)
    , _strokeLineJoin   :: !(Last SvgLineJoin)
    , _strokeMiterLimit :: !(Last Float)
    , _fillColor        :: !(Last SvgTexture)
    , _fillOpacity      :: !Float
    , _transform        :: !(Maybe [SvgTransformation])
    , _fillRule         :: !(Last SvgFillRule)
    , _attrClass        :: !(Last String)
    , _attrId           :: !(Maybe String)
    , _strokeOffset     :: !(Last SvgNumber)
    , _strokeDashArray  :: !(Last [SvgNumber])

    , _fontSize         :: !(Last SvgNumber)
    , _fontFamily       :: !(Last [String])
    , _fontStyle        :: !(Last SvgFontStyle)
    , _textAnchor       :: !(Last SvgTextAnchor)
    }
    deriving (Eq, Show)

makeClassy ''SvgDrawAttributes

data SvgPolyLine = SvgPolyLine
  { _svgPolyLineDrawAttributes :: SvgDrawAttributes 
  , _svgPolyLinePoints :: [Point]
  }
  deriving (Eq, Show)

makeClassy ''SvgPolyLine

defaultPolyLine :: SvgPolyLine
defaultPolyLine = SvgPolyLine
  { _svgPolyLineDrawAttributes = mempty
  , _svgPolyLinePoints = []
  }

instance WithSvgDrawAttributes SvgPolyLine where
    drawAttr = svgPolyLineDrawAttributes

data SvgPolygon = SvgPolygon
  { _svgPolygonDrawAttributes :: SvgDrawAttributes
  , _svgPolygonPoints :: [Point]
  }
  deriving (Eq, Show)

makeClassy ''SvgPolygon

instance WithSvgDrawAttributes SvgPolygon where
    drawAttr = svgPolygonDrawAttributes

defaultPolygon :: SvgPolygon
defaultPolygon = SvgPolygon
  { _svgPolygonDrawAttributes = mempty
  , _svgPolygonPoints = []
  }


data SvgLine = SvgLine
  { _svgLineDrawAttributes :: SvgDrawAttributes
  , _svgLinePoint1 :: SvgPoint
  , _svgLinePoint2 :: SvgPoint
  }
  deriving (Eq, Show)

makeClassy ''SvgLine

instance WithSvgDrawAttributes SvgLine where
    drawAttr = svgLineDrawAttributes

defaultLine :: SvgLine
defaultLine = SvgLine
  { _svgLineDrawAttributes = mempty
  , _svgLinePoint1 = zeroPoint
  , _svgLinePoint2 = zeroPoint
  }
  where zeroPoint = (SvgNum 0, SvgNum 0)

data SvgRectangle = SvgRectangle 
  { _svgRectDrawAttributes  :: SvgDrawAttributes
  , _svgRectUpperLeftCorner :: SvgPoint
  , _svgRectWidth           :: SvgNumber
  , _svgRectHeight          :: SvgNumber
  , _svgRectCornerRadius    :: (SvgNumber, SvgNumber)
  }
  deriving (Eq, Show)

makeClassy ''SvgRectangle

instance WithSvgDrawAttributes SvgRectangle where
    drawAttr = svgRectDrawAttributes

defaultRectangle :: SvgRectangle
defaultRectangle = SvgRectangle
  { _svgRectDrawAttributes  = mempty
  , _svgRectUpperLeftCorner = (SvgNum 0, SvgNum 0)
  , _svgRectWidth           = SvgNum 0
  , _svgRectHeight          = SvgNum 0
  , _svgRectCornerRadius    = (SvgNum 0, SvgNum 0)
  }

data SvgPathPrim = SvgPathPrim
  { _svgPathDrawAttributes :: SvgDrawAttributes
  , _svgPathDefinition :: [SvgPath]
  }
  deriving (Eq, Show)

makeClassy '' SvgPathPrim

instance WithSvgDrawAttributes SvgPathPrim where
    drawAttr = svgPathDrawAttributes

defaultPathPrim :: SvgPathPrim
defaultPathPrim = SvgPathPrim
  { _svgPathDrawAttributes = mempty
  , _svgPathDefinition = []
  }

data SvgGroup a = SvgGroup
  { _svgGroupDrawAttributes :: !SvgDrawAttributes
  , _svgGroupChildren  :: ![a]
  , _svgGroupViewBox   :: !(Maybe (Int, Int, Int, Int))
  }
  deriving (Eq, Show)

makeLenses ''SvgGroup

instance WithSvgDrawAttributes (SvgGroup a) where
    drawAttr = svgGroupDrawAttributes

newtype SvgSymbol a =
    SvgSymbol { _groupOfSymbol :: SvgGroup a }

makeLenses ''SvgSymbol

instance WithSvgDrawAttributes (SvgSymbol a) where
    drawAttr = groupOfSymbol . drawAttr

defaultGroup :: SvgGroup a
defaultGroup = SvgGroup
  { _svgGroupDrawAttributes = mempty
  , _svgGroupChildren  = []
  , _svgGroupViewBox = Nothing
  }

data SvgCircle = SvgCircle
  { _svgCircleDrawAttributes :: SvgDrawAttributes
  , _svgCircleCenter   :: SvgPoint
  , _svgCircleRadius   :: SvgNumber
  }
  deriving (Eq, Show)

makeClassy ''SvgCircle

instance WithSvgDrawAttributes SvgCircle where
    drawAttr = svgCircleDrawAttributes

defaultCircle :: SvgCircle
defaultCircle = SvgCircle
  { _svgCircleDrawAttributes = mempty
  , _svgCircleCenter = (SvgNum 0, SvgNum 0)
  , _svgCircleRadius = SvgNum 0
  }

data SvgEllipse = SvgEllipse
  { _svgEllipseDrawAttributes :: SvgDrawAttributes
  , _svgEllipseCenter :: SvgPoint
  , _svgEllipseXRadius :: SvgNumber
  , _svgEllipseYRadius :: SvgNumber
  }
  deriving (Eq, Show)

makeClassy ''SvgEllipse

instance WithSvgDrawAttributes SvgEllipse where
  drawAttr = svgEllipseDrawAttributes

defaultEllipse :: SvgEllipse
defaultEllipse = SvgEllipse
  { _svgEllipseDrawAttributes = mempty
  , _svgEllipseCenter = (SvgNum 0, SvgNum 0)
  , _svgEllipseXRadius = SvgNum 0
  , _svgEllipseYRadius = SvgNum 0
  }

data SvgUse = SvgUse
  { _svgUseBase   :: SvgPoint
  , _svgUseName   :: String
  , _svgUseWidth  :: Maybe SvgNumber
  , _svgUseHeight :: Maybe SvgNumber
  , _svgUseDrawAttributes :: SvgDrawAttributes
  }
  deriving (Eq, Show)

makeClassy ''SvgUse

instance WithSvgDrawAttributes SvgUse where
  drawAttr = svgUseDrawAttributes

defaultSvgUse :: SvgUse
defaultSvgUse = SvgUse
  { _svgUseBase   = (SvgNum 0, SvgNum 0)
  , _svgUseName   = ""
  , _svgUseWidth  = Nothing
  , _svgUseHeight = Nothing
  , _svgUseDrawAttributes = mempty
  }

data SvgTextInfo = SvgTextInfo
  { _svgTextInfoX      :: ![SvgNumber]
  , _svgTextInfoY      :: ![SvgNumber]
  , _svgTextInfoDX     :: ![SvgNumber]
  , _svgTextInfoDY     :: ![SvgNumber]
  , _svgTextInfoRotate :: ![Float]
  , _svgTextInfoLength :: !(Maybe SvgNumber)
  }
  deriving (Eq, Show)

instance Monoid SvgTextInfo where
  mempty = SvgTextInfo [] [] [] [] [] Nothing
  mappend (SvgTextInfo x1 y1 dx1 dy1 r1 l1)
          (SvgTextInfo x2 y2 dx2 dy2 r2 l2) =
    SvgTextInfo (x1 <> x2)   (y1 <> y2)
                (dx1 <> dx2) (dy1 <> dy2)
                (r1 <> r2)
                (getLast $ Last l1 <> Last l2)

makeClassy ''SvgTextInfo

defaultSvgTextInfo :: SvgTextInfo
defaultSvgTextInfo = mempty

data CharInfo = CharInfo
  { _svgCharX  :: Maybe SvgNumber
  , _svgCharY  :: Maybe SvgNumber
  , _svgCharDx :: Maybe SvgNumber
  , _svgCharDy :: Maybe SvgNumber
  , _svgCharRotate :: Maybe Float
  }
  deriving Show

emptyCharInfo :: CharInfo
emptyCharInfo = CharInfo
  { _svgCharX      = Nothing
  , _svgCharY      = Nothing
  , _svgCharDx     = Nothing
  , _svgCharDy     = Nothing
  , _svgCharRotate = Nothing
  }

repeatLast :: [a] -> [a]
repeatLast = go where
  go lst = case lst of
    [] -> []
    [x] -> repeat x
    (x:xs) -> x : go xs

infinitizeTextInfo :: SvgTextInfo -> SvgTextInfo
infinitizeTextInfo nfo = SvgTextInfo
  { _svgTextInfoX = xInfo
  , _svgTextInfoY = yInfo
  , _svgTextInfoDX = _svgTextInfoDX nfo ++ repeat (SvgNum 0)
  , _svgTextInfoDY = _svgTextInfoDY nfo ++ repeat (SvgNum 0)
  , _svgTextInfoRotate = repeatLast $ _svgTextInfoRotate nfo
  , _svgTextInfoLength = _svgTextInfoLength nfo
  }
  where
    (xInfo, yInfo) = case (_svgTextInfoX nfo, _svgTextInfoY nfo) of
        ( [],  []) -> ([], [])
        ([_], [_]) -> ([], [])
        ([x],  ys) -> (repeat x, repeatLast ys)
        ( xs, [y]) -> (repeatLast xs, repeat y)
        ( xs,  ys) -> (repeatLast xs, repeatLast ys)

mapTextInfoLists :: (forall a. [a] -> [a]) -> SvgTextInfo -> SvgTextInfo
mapTextInfoLists f val = SvgTextInfo
    { _svgTextInfoX      = f $ _svgTextInfoX val
    , _svgTextInfoY      = f $ _svgTextInfoY val
    , _svgTextInfoDX     = f $ _svgTextInfoDX val
    , _svgTextInfoDY     = f $ _svgTextInfoDY val
    , _svgTextInfoRotate = f $ _svgTextInfoRotate val
    , _svgTextInfoLength = _svgTextInfoLength val
    }

textInfoRests :: SvgTextInfo -> SvgTextInfo -> SvgTextInfo
              -> SvgTextInfo
textInfoRests this parent sub = SvgTextInfo
    { _svgTextInfoX      = decideWith _svgTextInfoX
    , _svgTextInfoY      = decideWith _svgTextInfoY
    , _svgTextInfoDX     = decideWith _svgTextInfoDX
    , _svgTextInfoDY     = decideWith _svgTextInfoDY
    , _svgTextInfoRotate = decideWith _svgTextInfoRotate
    , _svgTextInfoLength = _svgTextInfoLength parent
    }
  where
    decideWith f =
        decide (f this) (f parent) (f sub)

    decide that top ssub = case that of
       [] -> ssub
       [_] -> top
       _ -> ssub


unconsTextInfo :: SvgTextInfo -> (CharInfo, SvgTextInfo)
unconsTextInfo nfo = (charInfo, restText) where
  unconsInf lst = case lst of
     []     -> (Nothing, [])
     (x:xs) -> (Just x, xs)

  (xC, xRest) = unconsInf $ _svgTextInfoX nfo
  (yC, yRest) = unconsInf $ _svgTextInfoY nfo
  (dxC, dxRest) = unconsInf $ _svgTextInfoDX nfo
  (dyC, dyRest) = unconsInf $ _svgTextInfoDY nfo
  (rotateC, rotateRest) = unconsInf $ _svgTextInfoRotate nfo

  restText = SvgTextInfo
    { _svgTextInfoX      = xRest
    , _svgTextInfoY      = yRest
    , _svgTextInfoDX     = dxRest
    , _svgTextInfoDY     = dyRest
    , _svgTextInfoRotate = rotateRest
    , _svgTextInfoLength = _svgTextInfoLength nfo
    }

  charInfo = CharInfo
    { _svgCharX = xC
    , _svgCharY = yC
    , _svgCharDx = dxC
    , _svgCharDy = dyC
    , _svgCharRotate = rotateC
    }

data SvgTextSpanContent
    = SvgSpanText    !T.Text
    | SvgSpanTextRef !String
    | SvgSpanSub     !SvgTextSpan
    deriving (Eq, Show)

data SvgTextSpan = SvgTextSpan
  { _svgSpanInfo           :: !SvgTextInfo
  , _svgSpanDrawAttributes :: !SvgDrawAttributes
  , _svgSpanContent        :: ![SvgTextSpanContent]
  }
  deriving (Eq, Show)

makeClassy ''SvgTextSpan

defaultSvgTextSpan :: SvgTextSpan
defaultSvgTextSpan = SvgTextSpan
  { _svgSpanInfo = defaultSvgTextInfo
  , _svgSpanDrawAttributes = mempty
  , _svgSpanContent        = mempty
  }

data SvgTextPathMethod
  = SvgTextPathAlign
  | SvgTextPathStretch
  deriving (Eq, Show)

data SvgTextPathSpacing
  = SvgTextPathSpacingExact
  | SvgTextPathSpacingAuto
  deriving (Eq, Show)

data SvgTextPath = SvgTextPath
  { _svgTextPathStartOffset :: !SvgNumber
  , _svgTextPathName        :: !String
  , _svgTextPathData        :: ![SvgPath]
  , _svgTextPathMethod      :: !SvgTextPathMethod
  , _svgTextPathSpacing     :: !SvgTextPathSpacing
  }
  deriving (Eq, Show)

makeClassy ''SvgTextPath

defaultSvgTextPath :: SvgTextPath
defaultSvgTextPath = SvgTextPath
  { _svgTextPathStartOffset = SvgNum 0
  , _svgTextPathName        = mempty
  , _svgTextPathMethod      = SvgTextPathAlign
  , _svgTextPathSpacing     = SvgTextPathSpacingExact
  , _svgTextPathData        = []
  }

data SvgTextAdjust
  = SvgTextAdjustSpacing
  | SvgTextAdjustSpacingAndGlyphs
  deriving (Eq, Show)

data SvgText = SvgText
  { _svgTextAdjust   :: !SvgTextAdjust
  , _svgTextRoot     :: !SvgTextSpan
  }
  deriving (Eq, Show)

makeClassy ''SvgText

instance WithSvgDrawAttributes SvgText where
  drawAttr = svgTextRoot . svgSpanDrawAttributes

defaultSvgText :: SvgText
defaultSvgText = SvgText
  { _svgTextRoot = defaultSvgTextSpan
  , _svgTextAdjust = SvgTextAdjustSpacing
  }

data SvgTree
    = SvgNone
    | Use       !SvgUse             !SvgTree
    | Group     !(SvgGroup SvgTree)
    | Symbol    !(SvgGroup SvgTree)
    | Path      !SvgPathPrim
    | Circle    !SvgCircle
    | PolyLine  !SvgPolyLine
    | Polygon   !SvgPolygon
    | Ellipse   !SvgEllipse
    | Line      !SvgLine
    | Rectangle !SvgRectangle
    | TextArea  !(Maybe SvgTextPath) !SvgText
    deriving (Eq, Show)

appNode :: [[a]] -> a -> [[a]]
appNode [] e = [[e]]
appNode (curr:above) e = (e:curr) : above

zipSvgTree :: ([[SvgTree]] -> SvgTree) -> SvgTree -> SvgTree
zipSvgTree f = dig [] where
  dig prev e@SvgNone = f $ appNode prev e
  dig prev e@(Use u sub) =
      f . appNode prev . Use u $ dig ([] : appNode prev e) sub
  dig prev e@(Group g) =
      f . appNode prev . Group $ zipGroup (appNode prev e) g
  dig prev e@(Symbol g) =
      f . appNode prev . Symbol $ zipGroup (appNode prev e) g
  dig prev e@(Path _) = f $ appNode prev e
  dig prev e@(Circle _) = f $ appNode prev e
  dig prev e@(PolyLine _) = f $ appNode prev e
  dig prev e@(Polygon _) = f $ appNode prev e
  dig prev e@(Ellipse _) = f $ appNode prev e
  dig prev e@(Line _) = f $ appNode prev e
  dig prev e@(Rectangle _) = f $ appNode prev e
  dig prev e@(TextArea _ _) = f $ appNode prev e

  zipGroup prev g = g { _svgGroupChildren = updatedChildren }
    where
      groupChild = _svgGroupChildren g
      updatedChildren = 
        [dig (c:prev) child
            | (child, c) <- zip groupChild $ inits groupChild]


nameOfTree :: SvgTree -> T.Text
nameOfTree v =
  case v of
   SvgNone     -> ""
   Use _ _     -> "use"
   Group _     -> "g"
   Symbol _    -> "symbol"
   Path _      -> "path"
   Circle _    -> "circle"
   PolyLine _  -> "polyline"
   Polygon _   -> "polygon"
   Ellipse _   -> "ellipse"
   Line _      -> "line"
   Rectangle _ -> "rectangle"
   TextArea _ _ -> "text"

drawAttrOfTree :: SvgTree -> SvgDrawAttributes
drawAttrOfTree v = case v of
  SvgNone -> mempty
  Use e _ -> e ^. drawAttr
  Group e -> e ^. drawAttr
  Symbol e -> e ^. drawAttr
  Path e -> e ^. drawAttr
  Circle e -> e ^. drawAttr
  PolyLine e -> e ^. drawAttr
  Polygon e -> e ^. drawAttr
  Ellipse e -> e ^. drawAttr
  Line e -> e ^. drawAttr
  Rectangle e -> e ^. drawAttr
  TextArea _ e -> e ^. drawAttr

setDrawAttrOfTree :: SvgTree -> SvgDrawAttributes -> SvgTree
setDrawAttrOfTree v attr = case v of
  SvgNone -> SvgNone
  Use e t -> Use (e & drawAttr .~ attr) t
  Group e -> Group $ e & drawAttr .~ attr
  Symbol e -> Symbol $ e & drawAttr .~ attr
  Path e -> Path $ e & drawAttr .~ attr
  Circle e -> Circle $ e & drawAttr .~ attr
  PolyLine e -> PolyLine $ e & drawAttr .~ attr
  Polygon e -> Polygon $ e & drawAttr .~ attr
  Ellipse e -> Ellipse $ e & drawAttr .~ attr
  Line e -> Line $ e & drawAttr .~ attr
  Rectangle e -> Rectangle $ e & drawAttr .~ attr
  TextArea a e -> TextArea a $ e & drawAttr .~ attr

instance WithSvgDrawAttributes SvgTree where
    drawAttr = lens drawAttrOfTree setDrawAttrOfTree

data GradientUnits
    = GradientUserSpace
    | GradientBoundingBox
    deriving (Eq, Show)

data SvgSpread
    = SpreadRepeat
    | SpreadPad
    | SpreadReflect
    deriving (Eq, Show)

data SvgGradientStop = SvgGradientStop 
    { _gradientOffset :: Float
    , _gradientColor  :: PixelRGBA8
    }
    deriving (Eq, Show)

makeClassy ''SvgGradientStop

data SvgLinearGradient = SvgLinearGradient
    { _linearGradientUnits  :: GradientUnits
    , _linearGradientStart  :: SvgPoint
    , _linearGradientStop   :: SvgPoint
    , _linearGradientSpread :: SvgSpread
    , _linearGradientTransform :: [SvgTransformation]
    , _linearGradientStops  :: [SvgGradientStop]
    }
    deriving (Eq, Show)

makeClassy ''SvgLinearGradient

defaultLinearGradient :: SvgLinearGradient
defaultLinearGradient = SvgLinearGradient
  { _linearGradientUnits     = GradientBoundingBox
  , _linearGradientStart     = (SvgPercent 0, SvgPercent 0)
  , _linearGradientStop      = (SvgPercent 1, SvgPercent 0)
  , _linearGradientSpread    = SpreadPad
  , _linearGradientTransform = []
  , _linearGradientStops     = []
  }


data SvgRadialGradient = SvgRadialGradient
  { _radialGradientUnits   :: GradientUnits
  , _radialGradientCenter  :: SvgPoint
  , _radialGradientRadius  :: SvgNumber
  , _radialGradientFocusX  :: Maybe SvgNumber
  , _radialGradientFocusY  :: Maybe SvgNumber
  , _radialGradientSpread  :: SvgSpread
  , _radialGradientTransform :: [SvgTransformation]
  , _radialGradientStops   :: [SvgGradientStop]
  }
  deriving (Eq, Show)

makeClassy ''SvgRadialGradient

defaultRadialGradient :: SvgRadialGradient
defaultRadialGradient = SvgRadialGradient
  { _radialGradientUnits   = GradientBoundingBox
  , _radialGradientCenter  = (SvgPercent 0.5, SvgPercent 0.5)
  , _radialGradientRadius  = SvgPercent 0.5
  , _radialGradientFocusX  = Nothing
  , _radialGradientFocusY  = Nothing
  , _radialGradientSpread  = SpreadPad
  , _radialGradientTransform = []
  , _radialGradientStops   = []
  }

data SvgElement
    = ElementLinearGradient SvgLinearGradient
    | ElementRadialGradient SvgRadialGradient
    | ElementGeometry SvgTree
    deriving (Eq, Show)

data SvgDocument = SvgDocument
    { _svgViewBox     :: Maybe (Int, Int, Int, Int)
    , _svgWidth       :: Maybe Int
    , _svgHeight      :: Maybe Int
    , _svgElements    :: [SvgTree]
    , _svgDefinitions :: M.Map String SvgElement
    }
    deriving (Eq, Show)

svgDocumentSize :: SvgDocument -> (Int, Int)
svgDocumentSize SvgDocument { _svgWidth = Just w
                            , _svgHeight = Just h } = (w, h)
svgDocumentSize SvgDocument { _svgViewBox = Just (x1, y1, x2, y2) } =
    (abs $ x2 - x1, abs $ y2 - y1)
svgDocumentSize _ = (1, 1)

mayMerge :: Monoid a => Maybe a -> Maybe a -> Maybe a
mayMerge (Just a) (Just b) = Just $ mappend a b
mayMerge _ b@(Just _) = b
mayMerge a Nothing = a

instance Monoid SvgDrawAttributes where
    mempty = SvgDrawAttributes 
        { _strokeWidth      = Last Nothing
        , _strokeColor      = Last Nothing
        , _strokeOpacity    = 1.0
        , _strokeLineCap    = Last Nothing
        , _strokeLineJoin   = Last Nothing
        , _strokeMiterLimit = Last Nothing
        , _fillColor        = Last Nothing
        , _fillOpacity      = 1.0
        , _fontSize         = Last Nothing
        , _fontFamily       = Last Nothing
        , _fontStyle        = Last Nothing
        , _transform        = Nothing
        , _fillRule         = Last Nothing
        , _attrClass        = Last Nothing
        , _attrId           = Nothing
        , _strokeOffset     = Last Nothing
        , _strokeDashArray  = Last Nothing
        , _textAnchor       = Last Nothing
        }

    mappend a b = SvgDrawAttributes
        { _strokeWidth = (mappend `on` _strokeWidth) a b
        , _strokeColor =  (mappend `on` _strokeColor) a b
        , _strokeLineCap = (mappend `on` _strokeLineCap) a b
        , _strokeOpacity = ((*) `on` _strokeOpacity) a b
        , _strokeLineJoin = (mappend `on` _strokeLineJoin) a b
        , _strokeMiterLimit = (mappend `on` _strokeMiterLimit) a b
        , _fillColor =  (mappend `on` _fillColor) a b
        , _fillOpacity = ((*) `on` _fillOpacity) a b
        , _fontSize = (mappend `on` _fontSize) a b
        , _transform = (mayMerge `on` _transform) a b
        , _fillRule = (mappend `on` _fillRule) a b
        , _attrClass = (mappend `on` _attrClass) a b
        , _attrId = _attrId b
        , _strokeOffset = (mappend `on` _strokeOffset) a b
        , _strokeDashArray = (mappend `on` _strokeDashArray) a b
        , _fontFamily = (mappend `on` _fontFamily) a b
        , _fontStyle = (mappend `on` _fontStyle) a b
        , _textAnchor = (mappend `on` _textAnchor) a b
        }

