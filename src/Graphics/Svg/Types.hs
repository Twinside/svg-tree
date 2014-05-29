module Graphics.Svg.Types
    ( Coord
    , Origin( .. )
    , Point
    , SvgDocument( .. )
    , SvgDrawAttributes( .. )
    , SvgPath( .. )
    , SvgCap( .. )
    , SvgLineJoin( .. )
    , SvgTree( .. )
    , SvgNumber( .. )
    , SvgTransformation( .. )
    , SvgPoint
    , toSvgPoint
    , svgDocumentSize

    , isPathArc
    , isPathWithArc
    ) where

import Data.Function( on )
import Data.Monoid( Monoid( .. ) )
import Data.Foldable( Foldable )
import qualified Data.Foldable as F
import Codec.Picture( PixelRGBA8( .. ) )
import Graphics.Rasterific.Transformations
import Graphics.Rasterific

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

data SvgTree
    = SvgNone
    | Group SvgDrawAttributes [SvgTree]
    | Path SvgDrawAttributes [SvgPath]
    | Circle SvgDrawAttributes SvgPoint SvgNumber
    | Ellipse SvgDrawAttributes SvgPoint SvgNumber SvgNumber
    | Line SvgDrawAttributes SvgPoint SvgPoint
    | PolyLine SvgDrawAttributes [Point]
    | Polygon SvgDrawAttributes [Point]
    | Rectangle SvgDrawAttributes SvgPoint SvgNumber SvgNumber SvgNumber SvgNumber
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

data SvgDocument = SvgDocument
    { _svgViewBox  :: Maybe (Int, Int, Int, Int)
    , _svgWidth    :: Maybe Int
    , _svgHeight   :: Maybe Int
    , _svgElements :: [SvgTree]
    }
    deriving (Eq, Show)

svgDocumentSize :: SvgDocument -> (Int, Int)
svgDocumentSize SvgDocument { _svgWidth = Just w, _svgHeight = Just h } = (w, h)
svgDocumentSize SvgDocument { _svgViewBox = Just (x1, y1, x2, y2) } =
    (abs $ x2 - x1, abs $ y2 - y1)
svgDocumentSize _ = (1, 1)

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

data SvgDrawAttributes = SvgDrawAttributes
    { _strokeWidth      :: !(Maybe SvgNumber)
    , _strokeColor      :: !(Maybe PixelRGBA8)
    , _strokeOpacity    :: !(Maybe Float)
    , _strokeLineCap    :: !(Maybe SvgCap)
    , _strokeLineJoin   :: !(Maybe SvgLineJoin)
    , _strokeMiterLimit :: !(Maybe Float)
    , _fillColor        :: !(Maybe PixelRGBA8)
    , _fillOpacity      :: !(Maybe Float)
    , _transform        :: !(Maybe [SvgTransformation])
    }
    deriving (Eq, Show)

mayRight :: Maybe a -> Maybe a -> Maybe a
mayRight _ b@(Just _) = b
mayRight a Nothing = a

mayMerge :: Monoid a => Maybe a -> Maybe a -> Maybe a
mayMerge (Just a) (Just b) = Just $ mappend a b
mayMerge _ b@(Just _) = b
mayMerge a Nothing = a

instance Monoid SvgDrawAttributes where
    mempty =
        SvgDrawAttributes Nothing Nothing Nothing Nothing
                          Nothing Nothing Nothing Nothing
                          Nothing
    mappend a b = SvgDrawAttributes
        { _strokeWidth = (mayRight `on` _strokeWidth) a b
        , _strokeColor =  (mayRight `on` _strokeColor) a b
        , _strokeLineCap = (mayRight `on` _strokeLineCap) a b
        , _strokeOpacity = (mayRight `on` _strokeOpacity) a b
        , _strokeLineJoin = (mayRight `on` _strokeLineJoin) a b
        , _strokeMiterLimit = (mayRight `on` _strokeMiterLimit) a b
        , _fillColor =  (mayRight `on` _fillColor) a b
        , _fillOpacity = (mayRight `on` _fillOpacity) a b
        , _transform =  (mayMerge `on` _transform) a b
        }

