{-# LANGUAGE TemplateHaskell #-}
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

    , SvgRadialGradient
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

    , SvgCircle( .. )
    , defaultCircle
    , HasSvgCircle( .. )

    , SvgEllipse( .. )
    , defaultEllipse
    , HasSvgEllipse( .. )

    , WithSvgDrawAttributes( .. )
    , isPathArc
    , isPathWithArc
    ) where

import Data.Function( on )
import qualified Data.Map as M
import Data.Monoid( Monoid( .. ) )
import Data.Foldable( Foldable )
import qualified Data.Foldable as F
import Codec.Picture( PixelRGBA8( .. ) )
import Graphics.Rasterific.Transformations
import Graphics.Rasterific hiding (Path, Line)
import Control.Lens

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

data SvgDrawAttributes = SvgDrawAttributes
    { _strokeWidth      :: !(Maybe SvgNumber)
    , _strokeColor      :: !(Maybe SvgTexture)
    , _strokeOpacity    :: !Float
    , _strokeLineCap    :: !(Maybe SvgCap)
    , _strokeLineJoin   :: !(Maybe SvgLineJoin)
    , _strokeMiterLimit :: !(Maybe Float)
    , _fillColor        :: !(Maybe SvgTexture)
    , _fillOpacity      :: !Float
    , _fontSize         :: !(Maybe Float)
    , _transform        :: (Maybe [SvgTransformation])
    , _fillRule         :: !(Maybe SvgFillRule)
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
  }
  deriving (Eq, Show)

makeLenses ''SvgGroup

instance WithSvgDrawAttributes (SvgGroup a) where
    drawAttr = svgGroupDrawAttributes

defaultGroup :: SvgGroup a
defaultGroup = SvgGroup
  { _svgGroupDrawAttributes = mempty
  , _svgGroupChildren  = []
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

data SvgTree
    = SvgNone
    | Use !String !SvgTree
    | Group !(SvgGroup SvgTree)
    | Path !SvgPathPrim
    | Circle !SvgCircle
    | PolyLine !SvgPolyLine
    | Polygon !SvgPolygon
    | Ellipse !SvgEllipse
    | Line !SvgLine
    | Rectangle !SvgRectangle
    deriving (Eq, Show)

drawAttrOfTree :: SvgTree -> SvgDrawAttributes
drawAttrOfTree v = case v of
  SvgNone -> mempty
  Use _ t -> drawAttrOfTree t
  Group e -> e ^. drawAttr
  Path e -> e ^. drawAttr
  Circle e -> e ^. drawAttr
  PolyLine e -> e ^. drawAttr
  Polygon e -> e ^. drawAttr
  Ellipse e -> e ^. drawAttr
  Line e -> e ^. drawAttr
  Rectangle e -> e ^. drawAttr

setDrawAttrOfTree :: SvgTree -> SvgDrawAttributes -> SvgTree
setDrawAttrOfTree v attr = case v of
  SvgNone -> SvgNone
  Use n t -> Use n $ setDrawAttrOfTree t attr
  Group e -> Group $ e & drawAttr .~ attr
  Path e -> Path $ e & drawAttr .~ attr
  Circle e -> Circle $ e & drawAttr .~ attr
  PolyLine e -> PolyLine $ e & drawAttr .~ attr
  Polygon e -> Polygon $ e & drawAttr .~ attr
  Ellipse e -> Ellipse $ e & drawAttr .~ attr
  Line e -> Line $ e & drawAttr .~ attr
  Rectangle e -> Rectangle $ e & drawAttr .~ attr

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

mayRight :: Maybe a -> Maybe a -> Maybe a
mayRight _ b@(Just _) = b
mayRight a Nothing = a

mayMerge :: Monoid a => Maybe a -> Maybe a -> Maybe a
mayMerge (Just a) (Just b) = Just $ mappend a b
mayMerge _ b@(Just _) = b
mayMerge a Nothing = a

instance Monoid SvgDrawAttributes where
    mempty = SvgDrawAttributes 
        { _strokeWidth      = Nothing
        , _strokeColor      = Nothing
        , _strokeOpacity    = 1.0
        , _strokeLineCap    = Nothing
        , _strokeLineJoin   = Nothing
        , _strokeMiterLimit = Nothing
        , _fillColor        = Nothing
        , _fillOpacity      = 1.0
        , _fontSize         = Nothing
        , _transform        = Nothing
        , _fillRule         = Nothing
        }

    mappend a b = SvgDrawAttributes
        { _strokeWidth = (mayRight `on` _strokeWidth) a b
        , _strokeColor =  (mayRight `on` _strokeColor) a b
        , _strokeLineCap = (mayRight `on` _strokeLineCap) a b
        , _strokeOpacity = ((*) `on` _strokeOpacity) a b
        , _strokeLineJoin = (mayRight `on` _strokeLineJoin) a b
        , _strokeMiterLimit = (mayRight `on` _strokeMiterLimit) a b
        , _fillColor =  (mayRight `on` _fillColor) a b
        , _fillOpacity = ((*) `on` _fillOpacity) a b
        , _fontSize = (mayRight `on` _fontSize) a b
        , _transform = (mayMerge `on` _transform) a b
        , _fillRule = (mayRight `on` _fillRule) a b
        }

