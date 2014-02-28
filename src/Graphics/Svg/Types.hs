module Graphics.Svg.Types
    ( Coord
    , Point
    , Origin( .. )
    , SvgPath( .. )
    , Transform( .. )
    ) where

import Data.Monoid( Monoid( .. ) )

type Coord = Double

type Point = (Coord, Coord)

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

-- | Represent an SVG transformation matrix
data Transform = Transform
    { _transformA :: {-# UNPACK #-} !Double
    , _transformC :: {-# UNPACK #-} !Double
    , _transformE :: {-# UNPACK #-} !Double -- ^ X translation

    , _transformB :: {-# UNPACK #-} !Double
    , _transformD :: {-# UNPACK #-} !Double
    , _transformF :: {-# UNPACK #-} !Double -- ^ Y translation
    }
    deriving (Eq, Show)

transformCombine :: Transform -> Transform -> Transform
transformCombine (Transform a c e
                            b d f)

                 (Transform a' c' e'
                            b' d' f') =
    Transform (a * a' + c * b' {- below b' is zero -})
              (a * c' + c * d' {- below d' is zero -})
              (a * e' + c * f' + e {- below f' is one -})

              (b * a' + d * b' {- below b' is zero -})
              (b * c' + d * d' {- below d' is zero -})
              (b * e' + d * f' + f {- below f' is one -})

instance Monoid Transform where
    mappend = transformCombine
    mempty = Transform 1 0 0
                       0 1 0

