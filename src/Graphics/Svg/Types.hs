{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | This module define all the types used in the definition
-- of a svg scene.
--
-- Most of the types are lensified.
module Graphics.Svg.Types
    ( -- * Basic building types
      Coord
    , Origin( .. )
    , Point
    , RPoint
    , Path( .. )
    , Transformation( .. )
      -- ** Building helpers
    , toPoint
    , serializeNumber
    , serializeTransformation
    , serializeTransformations

      -- * Drawing control types
    , Cap( .. )
    , LineJoin( .. )
    , Tree( .. )
    , Number( .. )
    , Spread( .. )
    , Texture( .. )
    , Element( .. )
    , FillRule( .. )
    , FontStyle( .. )

    , WithDefaultSvg( .. )

      -- * Main type
    , Document( .. )
    , documentSize

      -- * Drawing attributes
    , DrawAttributes( .. )
    , HasDrawAttributes( .. )
    , WithDrawAttributes( .. )

      -- * Marker definition
    , MarkerAttribute( .. )
    , Marker( .. )
    , MarkerOrientation( .. )
    , MarkerUnit( .. )
    , HasMarker( .. )
    , defaultMarker

      -- * Gradient definition
    , GradientUnits( .. )
    , GradientStop( .. )
    , HasGradientStop( .. )

    , LinearGradient( .. )
    , defaultLinearGradient
    , HasLinearGradient( .. )

    , RadialGradient( .. )
    , defaultRadialGradient
    , HasRadialGradient( .. )

      -- * Pattern definition
    , Pattern( .. )
    , PatternUnit( .. )
    , defaultPattern
    , HasPattern( .. )

      -- * SVG drawing primitives
    , Rectangle( .. )
    , defaultRectangle
    , HasRectangle( .. )

    , Line( .. )
    , defaultLine
    , HasLine( .. )

    , Polygon( .. )
    , defaultPolygon
    , HasPolygon( .. )

    , PolyLine( .. )
    , defaultPolyLine
    , HasPolyLine( .. )

    , PathPrim( .. )
    , defaultPathPrim
    , HasPathPrim( .. )

    , Circle( .. )
    , defaultCircle
    , HasCircle( .. )

    , Ellipse( .. )
    , defaultEllipse
    , HasEllipse( .. )

    , Use( .. )
    , defaultUse
    , HasUse( .. )

      -- * Grouping primitives
    , Group( .. )
    , defaultGroup
    , groupDrawAttributes
    , groupChildren
    , groupViewBox

    , Symbol( .. )
    , groupOfSymbol

      -- * Text related types
    , Text( .. )
    , defaultText
    , HasText( .. )

    , TextPath( .. )
    , defaultTextPath 
    , HasTextPath( .. )

    , TextPathSpacing( .. )
    , TextPathMethod( .. )
    , TextAnchor( .. )

    , TextSpanContent( .. )

    , TextSpan( .. )
    , defaultTextSpan
    , HasTextSpan( .. )

    , TextInfo( .. )
    , defaultTextInfo
    , HasTextInfo( .. )

    , TextAdjust( .. )

      -- * MISC functions
    , isPathArc
    , isPathWithArc
    , nameOfTree
    , zipTree
    ) where

import Data.Function( on )
import Data.List( inits )
import qualified Data.Map as M
import Data.Monoid( Monoid( .. ), Last( .. ), (<>) )
import Data.Foldable( Foldable )
import qualified Data.Foldable as F
import qualified Data.Text as T
import Codec.Picture( PixelRGBA8( .. ) )
import Control.Lens( Lens'
                   , lens
                   , makeClassy
                   , makeLenses
                   , view
                   , (^.)
                   , (&)
                   , (.~)
                   )
import Graphics.Svg.CssTypes
import Linear hiding ( angle )

import Text.Printf

-- | Basic coordiante type.
type Coord = Float

-- | Real Point, fully determined and not
-- dependant of the rendering context.
type RPoint = V2 Coord

-- | Possibly context dependant point.
type Point = (Number, Number)

-- | Tell if a path command is absolute (in the current
-- user coordiante) or relative to the previous poitn.
data Origin
  = OriginAbsolute
  | OriginRelative
  deriving (Eq, Show)

-- | Path command definition.
data Path
      -- | 'M' or 'm' command
    = MoveTo Origin [RPoint]
      -- | Line to, 'L' or 'l' Svg path command.
    | LineTo Origin [RPoint]

      -- | Equivalent to the 'H' or 'h' svg path command.
    | HorizontalTo  Origin [Coord]
      -- | Equivalent to the 'V' or 'v' svg path command.
    | VerticalTo    Origin [Coord]

    -- | Cubic bezier, 'C' or 'c' command
    | CurveTo  Origin [(RPoint, RPoint, RPoint)]
    -- | Smooth cubic bezier, equivalent to 'S' or 's' command
    | SmoothCurveTo  Origin [(RPoint, RPoint)]
    -- | Quadratic bezier, 'Q' or 'q' command
    | QuadraticBezier  Origin [(RPoint, RPoint)]
    -- | Quadratic bezier, 'T' or 't' command
    | SmoothQuadraticBezierCurveTo  Origin [RPoint]
      -- | Eliptical arc, 'A' or 'a' command.
    | ElipticalArc  Origin [(Coord, Coord, Coord, Coord, Coord, RPoint)]
      -- | Close the path, 'Z' or 'z' svg path command.
    | EndPath
    deriving (Eq, Show)

toPoint :: Number -> Number -> Point
toPoint = (,)

isPathArc :: Path -> Bool
isPathArc (ElipticalArc _ _) = True
isPathArc _ = False

isPathWithArc :: Foldable f => f Path -> Bool
isPathWithArc = F.any isPathArc


data Cap
    = CapRound
    | CapButt
    | CapSquare
    deriving (Eq, Show)

data LineJoin
    = JoinMiter
    | JoinBevel
    | JoinRound
    deriving (Eq, Show)

data Texture
    = ColorRef   PixelRGBA8
    | TextureRef String
    | FillNone
    deriving (Eq, Show)

data FillRule
    = FillEvenOdd
    | FillNonZero
    deriving (Eq, Show)

data Transformation
    = TransformMatrix Coord Coord Coord
                      Coord Coord Coord
    | Translate Float Float
    | Scale Float (Maybe Float)
    | Rotate Float (Maybe (Float, Float))
    | SkewX Float
    | SkewY Float
    | TransformUnknown
    deriving (Eq, Show)

serializeTransformation :: Transformation -> String
serializeTransformation t = case t of
  TransformUnknown -> ""
  TransformMatrix a b c d e f ->
      printf "matrix(%g, %g, %g, %g, %g, %g)" a b c d e f
  Translate x y -> printf "translate(%g, %g)" x y
  Scale x Nothing -> printf "scale(%g)" x
  Scale x (Just y) -> printf "scale(%g, %g)" x y
  Rotate angle Nothing -> printf "rotate(%g)" angle
  Rotate angle (Just (x, y))-> printf "rotate(%g, %g, %g)" angle x y
  SkewX x -> printf "skewX(%g)" x
  SkewY y -> printf "skewY(%g)" y

serializeTransformations :: [Transformation] -> String
serializeTransformations =
    unwords . fmap serializeTransformation

-- | Class helping find the drawing attributes for all
-- the SVG attributes.
class WithDrawAttributes a where
    -- | Lens which can be used to read/write primitives.
    drawAttr :: Lens' a DrawAttributes

-- | Define an empty 'default' element for the SVG tree.
-- It is used as base when parsing the element from XML.
class WithDefaultSvg a where
    -- | The default element.
    defaultSvg :: a

-- | Classify the font style, used to search a matching
-- font in the FontCache.
data FontStyle
  = FontStyleNormal
  | FontStyleItalic
  | FontStyleOblique
  deriving (Eq, Show)

-- | Tell where to anchor the text, where the position
-- given is realative to the text.
data TextAnchor
    -- | The text with left aligned, or start at the postion
    -- If the point is the '*' then the text will be printed
    -- this way:
    --
    -- >  *THE_TEXT_TO_PRINT
    --
  = TextAnchorStart
    -- | The text is middle aligned, so the text will be at
    -- the left and right of the position:
    --
    -- >   THE_TEXT*TO_PRINT
    --
  | TextAnchorMiddle
    -- | The text is right aligned.
    --
    -- >   THE_TEXT_TO_PRINT*
    --
  | TextAnchorEnd
  deriving (Eq, Show)

data MarkerAttribute
  = MarkerNone
  | MarkerRef String
  deriving (Eq, Show)

-- | This type define how to draw any primitives,
-- which color to use, how to stroke the primitives
-- and the potential transformations to use.
--
-- All these attributes are propagated to the children.
data DrawAttributes = DrawAttributes
    { _strokeWidth      :: !(Last Number)
    , _strokeColor      :: !(Last Texture)
    , _strokeOpacity    :: !(Maybe Float)
    , _strokeLineCap    :: !(Last Cap)
    , _strokeLineJoin   :: !(Last LineJoin)
    , _strokeMiterLimit :: !(Last Float)
    , _fillColor        :: !(Last Texture)
    , _fillOpacity      :: !(Maybe Float)
    , _transform        :: !(Maybe [Transformation])
    , _fillRule         :: !(Last FillRule)
    , _attrClass        :: !(Last String)
    , _attrId           :: !(Maybe String)
    , _strokeOffset     :: !(Last Number)
    , _strokeDashArray  :: !(Last [Number])

    , _fontSize         :: !(Last Number)
    , _fontFamily       :: !(Last [String])
    , _fontStyle        :: !(Last FontStyle)
    , _textAnchor       :: !(Last TextAnchor)

    , _markerStart      :: !(Last MarkerAttribute)
    , _markerMid        :: !(Last MarkerAttribute)
    , _markerEnd        :: !(Last MarkerAttribute)
    }
    deriving (Eq, Show)

makeClassy ''DrawAttributes

-- | This primitive describe an unclosed suite of
-- segments.
data PolyLine = PolyLine
  { -- | drawing attributes of the polyline.
    _polyLineDrawAttributes :: DrawAttributes 
    -- | Geometry definition of the polyline.
  , _polyLinePoints :: [RPoint]
  }
  deriving (Eq, Show)

makeClassy ''PolyLine

defaultPolyLine :: PolyLine
defaultPolyLine = PolyLine
  { _polyLineDrawAttributes = mempty
  , _polyLinePoints = []
  }

instance WithDrawAttributes PolyLine where
    drawAttr = polyLineDrawAttributes

data Polygon = Polygon
  { _polygonDrawAttributes :: DrawAttributes
  , _polygonPoints :: [RPoint]
  }
  deriving (Eq, Show)

makeClassy ''Polygon

instance WithDrawAttributes Polygon where
    drawAttr = polygonDrawAttributes

defaultPolygon :: Polygon
defaultPolygon = Polygon
  { _polygonDrawAttributes = mempty
  , _polygonPoints = []
  }


data Line = Line
  { _lineDrawAttributes :: DrawAttributes
  , _linePoint1 :: Point
  , _linePoint2 :: Point
  }
  deriving (Eq, Show)

makeClassy ''Line

instance WithDrawAttributes Line where
    drawAttr = lineDrawAttributes

defaultLine :: Line
defaultLine = Line
  { _lineDrawAttributes = mempty
  , _linePoint1 = zeroPoint
  , _linePoint2 = zeroPoint
  }
  where zeroPoint = (Num 0, Num 0)

data Rectangle = Rectangle 
  { _rectDrawAttributes  :: DrawAttributes
  , _rectUpperLeftCorner :: Point
  , _rectWidth           :: Number
  , _rectHeight          :: Number
  , _rectCornerRadius    :: (Number, Number)
  }
  deriving (Eq, Show)

makeClassy ''Rectangle

instance WithDrawAttributes Rectangle where
    drawAttr = rectDrawAttributes

defaultRectangle :: Rectangle
defaultRectangle = Rectangle
  { _rectDrawAttributes  = mempty
  , _rectUpperLeftCorner = (Num 0, Num 0)
  , _rectWidth           = Num 0
  , _rectHeight          = Num 0
  , _rectCornerRadius    = (Num 0, Num 0)
  }

data PathPrim = PathPrim
  { _pathDrawAttributes :: DrawAttributes
  , _pathDefinition :: [Path]
  }
  deriving (Eq, Show)

makeClassy '' PathPrim

instance WithDrawAttributes PathPrim where
    drawAttr = pathDrawAttributes

defaultPathPrim :: PathPrim
defaultPathPrim = PathPrim
  { _pathDrawAttributes = mempty
  , _pathDefinition = []
  }

data Group a = Group
  { _groupDrawAttributes :: !DrawAttributes
  , _groupChildren  :: ![a]
  , _groupViewBox   :: !(Maybe (Int, Int, Int, Int))
  }
  deriving (Eq, Show)

makeLenses ''Group

instance WithDrawAttributes (Group a) where
    drawAttr = groupDrawAttributes

newtype Symbol a =
    Symbol { _groupOfSymbol :: Group a }

makeLenses ''Symbol

instance WithDrawAttributes (Symbol a) where
    drawAttr = groupOfSymbol . drawAttr

defaultGroup :: Group a
defaultGroup = Group
  { _groupDrawAttributes = mempty
  , _groupChildren  = []
  , _groupViewBox = Nothing
  }

data Circle = Circle
  { _circleDrawAttributes :: DrawAttributes
  , _circleCenter   :: Point
  , _circleRadius   :: Number
  }
  deriving (Eq, Show)

makeClassy ''Circle

instance WithDrawAttributes Circle where
    drawAttr = circleDrawAttributes

defaultCircle :: Circle
defaultCircle = Circle
  { _circleDrawAttributes = mempty
  , _circleCenter = (Num 0, Num 0)
  , _circleRadius = Num 0
  }

data Ellipse = Ellipse
  { _ellipseDrawAttributes :: DrawAttributes
  , _ellipseCenter :: Point
  , _ellipseXRadius :: Number
  , _ellipseYRadius :: Number
  }
  deriving (Eq, Show)

makeClassy ''Ellipse

instance WithDrawAttributes Ellipse where
  drawAttr = ellipseDrawAttributes

defaultEllipse :: Ellipse
defaultEllipse = Ellipse
  { _ellipseDrawAttributes = mempty
  , _ellipseCenter = (Num 0, Num 0)
  , _ellipseXRadius = Num 0
  , _ellipseYRadius = Num 0
  }

data Use = Use
  { _useBase   :: Point
  , _useName   :: String
  , _useWidth  :: Maybe Number
  , _useHeight :: Maybe Number
  , _useDrawAttributes :: DrawAttributes
  }
  deriving (Eq, Show)

makeClassy ''Use

instance WithDrawAttributes Use where
  drawAttr = useDrawAttributes

defaultUse :: Use
defaultUse = Use
  { _useBase   = (Num 0, Num 0)
  , _useName   = ""
  , _useWidth  = Nothing
  , _useHeight = Nothing
  , _useDrawAttributes = mempty
  }

data TextInfo = TextInfo
  { _textInfoX      :: ![Number]
  , _textInfoY      :: ![Number]
  , _textInfoDX     :: ![Number]
  , _textInfoDY     :: ![Number]
  , _textInfoRotate :: ![Float]
  , _textInfoLength :: !(Maybe Number)
  }
  deriving (Eq, Show)

instance Monoid TextInfo where
  mempty = TextInfo [] [] [] [] [] Nothing
  mappend (TextInfo x1 y1 dx1 dy1 r1 l1)
          (TextInfo x2 y2 dx2 dy2 r2 l2) =
    TextInfo (x1 <> x2)   (y1 <> y2)
                (dx1 <> dx2) (dy1 <> dy2)
                (r1 <> r2)
                (getLast $ Last l1 <> Last l2)

makeClassy ''TextInfo

defaultTextInfo :: TextInfo
defaultTextInfo = mempty

data TextSpanContent
    = SpanText    !T.Text
    | SpanTextRef !String
    | SpanSub     !TextSpan
    deriving (Eq, Show)

data TextSpan = TextSpan
  { _spanInfo           :: !TextInfo
  , _spanDrawAttributes :: !DrawAttributes
  , _spanContent        :: ![TextSpanContent]
  }
  deriving (Eq, Show)

makeClassy ''TextSpan

defaultTextSpan :: TextSpan
defaultTextSpan = TextSpan
  { _spanInfo = defaultTextInfo
  , _spanDrawAttributes = mempty
  , _spanContent        = mempty
  }

data TextPathMethod
  = TextPathAlign
  | TextPathStretch
  deriving (Eq, Show)

data TextPathSpacing
  = TextPathSpacingExact
  | TextPathSpacingAuto
  deriving (Eq, Show)

data TextPath = TextPath
  { _textPathStartOffset :: !Number
  , _textPathName        :: !String
  , _textPathData        :: ![Path]
  , _textPathMethod      :: !TextPathMethod
  , _textPathSpacing     :: !TextPathSpacing
  }
  deriving (Eq, Show)

makeClassy ''TextPath

defaultTextPath :: TextPath
defaultTextPath = TextPath
  { _textPathStartOffset = Num 0
  , _textPathName        = mempty
  , _textPathMethod      = TextPathAlign
  , _textPathSpacing     = TextPathSpacingExact
  , _textPathData        = []
  }

data TextAdjust
  = TextAdjustSpacing
  | TextAdjustSpacingAndGlyphs
  deriving (Eq, Show)

data Text = Text
  { _textAdjust   :: !TextAdjust
  , _textRoot     :: !TextSpan
  }
  deriving (Eq, Show)

makeClassy ''Text

instance WithDrawAttributes Text where
  drawAttr = textRoot . spanDrawAttributes

defaultText :: Text
defaultText = Text
  { _textRoot = defaultTextSpan
  , _textAdjust = TextAdjustSpacing
  }

data Tree
    = None
    | UseTree       !Use  !Tree
    | GroupTree     !(Group Tree)
    | SymbolTree    !(Group Tree)
    | Path          !PathPrim
    | CircleTree    !Circle
    | PolyLineTree  !PolyLine
    | PolygonTree   !Polygon
    | EllipseTree   !Ellipse
    | LineTree      !Line
    | RectangleTree !Rectangle
    | TextArea      !(Maybe TextPath) !Text
    deriving (Eq, Show)

data MarkerOrientation
  = OrientationAuto
  | OrientationAngle Coord
  deriving (Eq, Show)

data MarkerUnit
  = MarkerUnitStrokeWidth
  | MarkerUnitUserSpaceOnUse
  deriving (Eq, Show)

data Marker = Marker
  { _markerDrawAttributes :: DrawAttributes
  , _markerRefPoint :: (Number, Number)
  , _markerWidth    :: Number
  , _markerHeight   :: Number
  , _markerOrient   :: Maybe MarkerOrientation
  , _markerUnits    :: Maybe MarkerUnit
  , _markerElements :: [Tree]
  }
  deriving (Eq, Show)

makeClassy ''Marker

instance WithDrawAttributes Marker where
    drawAttr = markerDrawAttributes

defaultMarker :: Marker
defaultMarker = Marker
  { _markerDrawAttributes = mempty
  , _markerRefPoint = (Num 0, Num 0)
  , _markerWidth = Num 0
  , _markerHeight = Num 0
  , _markerOrient = Nothing -- MarkerOrientation
  , _markerUnits = Nothing -- MarkerUnitStrokeWidth
  , _markerElements = []
  }

appNode :: [[a]] -> a -> [[a]]
appNode [] e = [[e]]
appNode (curr:above) e = (e:curr) : above

zipTree :: ([[Tree]] -> Tree) -> Tree -> Tree
zipTree f = dig [] where
  dig prev e@None = f $ appNode prev e
  dig prev e@(UseTree u sub) =
      f . appNode prev . UseTree u $ dig ([] : appNode prev e) sub
  dig prev e@(GroupTree g) =
      f . appNode prev . GroupTree $ zipGroup (appNode prev e) g
  dig prev e@(SymbolTree g) =
      f . appNode prev . SymbolTree $ zipGroup (appNode prev e) g
  dig prev e@(Path _) = f $ appNode prev e
  dig prev e@(CircleTree _) = f $ appNode prev e
  dig prev e@(PolyLineTree _) = f $ appNode prev e
  dig prev e@(PolygonTree _) = f $ appNode prev e
  dig prev e@(EllipseTree _) = f $ appNode prev e
  dig prev e@(LineTree _) = f $ appNode prev e
  dig prev e@(RectangleTree _) = f $ appNode prev e
  dig prev e@(TextArea _ _) = f $ appNode prev e

  zipGroup prev g = g { _groupChildren = updatedChildren }
    where
      groupChild = _groupChildren g
      updatedChildren = 
        [dig (c:prev) child
            | (child, c) <- zip groupChild $ inits groupChild]


nameOfTree :: Tree -> T.Text
nameOfTree v =
  case v of
   None     -> ""
   UseTree _ _     -> "use"
   GroupTree _     -> "g"
   SymbolTree _    -> "symbol"
   Path _      -> "path"
   CircleTree _    -> "circle"
   PolyLineTree _  -> "polyline"
   PolygonTree _   -> "polygon"
   EllipseTree _   -> "ellipse"
   LineTree _      -> "line"
   RectangleTree _ -> "rectangle"
   TextArea    _ _ -> "text"

drawAttrOfTree :: Tree -> DrawAttributes
drawAttrOfTree v = case v of
  None -> mempty
  UseTree e _ -> e ^. drawAttr
  GroupTree e -> e ^. drawAttr
  SymbolTree e -> e ^. drawAttr
  Path e -> e ^. drawAttr
  CircleTree e -> e ^. drawAttr
  PolyLineTree e -> e ^. drawAttr
  PolygonTree e -> e ^. drawAttr
  EllipseTree e -> e ^. drawAttr
  LineTree e -> e ^. drawAttr
  RectangleTree e -> e ^. drawAttr
  TextArea _ e -> e ^. drawAttr

setDrawAttrOfTree :: Tree -> DrawAttributes -> Tree
setDrawAttrOfTree v attr = case v of
  None -> None
  UseTree e t -> UseTree (e & drawAttr .~ attr) t
  GroupTree e -> GroupTree $ e & drawAttr .~ attr
  SymbolTree e -> SymbolTree $ e & drawAttr .~ attr
  Path e -> Path $ e & drawAttr .~ attr
  CircleTree e -> CircleTree $ e & drawAttr .~ attr
  PolyLineTree e -> PolyLineTree $ e & drawAttr .~ attr
  PolygonTree e -> PolygonTree $ e & drawAttr .~ attr
  EllipseTree e -> EllipseTree $ e & drawAttr .~ attr
  LineTree e -> LineTree $ e & drawAttr .~ attr
  RectangleTree e -> RectangleTree $ e & drawAttr .~ attr
  TextArea a e -> TextArea a $ e & drawAttr .~ attr

instance WithDrawAttributes Tree where
    drawAttr = lens drawAttrOfTree setDrawAttrOfTree

data GradientUnits
    = GradientUserSpace
    | GradientBoundingBox
    deriving (Eq, Show)

data Spread
    = SpreadRepeat
    | SpreadPad
    | SpreadReflect
    deriving (Eq, Show)

data GradientStop = GradientStop 
    { _gradientOffset :: Float
    , _gradientColor  :: PixelRGBA8
    }
    deriving (Eq, Show)

makeClassy ''GradientStop

data LinearGradient = LinearGradient
    { _linearGradientUnits  :: GradientUnits
    , _linearGradientStart  :: Point
    , _linearGradientStop   :: Point
    , _linearGradientSpread :: Spread
    , _linearGradientTransform :: [Transformation]
    , _linearGradientStops  :: [GradientStop]
    }
    deriving (Eq, Show)

makeClassy ''LinearGradient

defaultLinearGradient :: LinearGradient
defaultLinearGradient = LinearGradient
  { _linearGradientUnits     = GradientBoundingBox
  , _linearGradientStart     = (Percent 0, Percent 0)
  , _linearGradientStop      = (Percent 1, Percent 0)
  , _linearGradientSpread    = SpreadPad
  , _linearGradientTransform = []
  , _linearGradientStops     = []
  }


data RadialGradient = RadialGradient
  { _radialGradientUnits   :: GradientUnits
  , _radialGradientCenter  :: Point
  , _radialGradientRadius  :: Number
  , _radialGradientFocusX  :: Maybe Number
  , _radialGradientFocusY  :: Maybe Number
  , _radialGradientSpread  :: Spread
  , _radialGradientTransform :: [Transformation]
  , _radialGradientStops   :: [GradientStop]
  }
  deriving (Eq, Show)

makeClassy ''RadialGradient

defaultRadialGradient :: RadialGradient
defaultRadialGradient = RadialGradient
  { _radialGradientUnits   = GradientBoundingBox
  , _radialGradientCenter  = (Percent 0.5, Percent 0.5)
  , _radialGradientRadius  = Percent 0.5
  , _radialGradientFocusX  = Nothing
  , _radialGradientFocusY  = Nothing
  , _radialGradientSpread  = SpreadPad
  , _radialGradientTransform = []
  , _radialGradientStops   = []
  }

data PatternUnit
  = PatternUnitUserSpaceOnUse
  | PatternUnitObjectBoundingBox
  deriving (Eq, Show)

data Pattern = Pattern
    { _patternDrawAttributes :: DrawAttributes
    , _patternViewBox  :: Maybe (Int, Int, Int, Int)
    , _patternWidth    :: Number
    , _patternHeight   :: Number
    , _patternPos      :: Point
    , _patternElements :: [Tree]
    , _patternUnit     :: PatternUnit
    }
    deriving Show

makeClassy ''Pattern

instance WithDrawAttributes Pattern where
    drawAttr = patternDrawAttributes

defaultPattern :: Pattern
defaultPattern = Pattern
  { _patternViewBox  = Nothing
  , _patternWidth    = Num 0
  , _patternHeight   = Num 0
  , _patternPos      = (Num 0, Num 0)
  , _patternElements = []
  , _patternUnit = PatternUnitObjectBoundingBox
  , _patternDrawAttributes = mempty
  }

data Element
    = ElementLinearGradient LinearGradient
    | ElementRadialGradient RadialGradient
    | ElementGeometry Tree
    | ElementPattern  Pattern
    | ElementMarker Marker
    deriving Show

{-
instance Show SvgElement where
    show (ElementLinearGradient grad) =
        "ElementLinearGradient " ++ show grad
    show (ElementRadialGradient grad) =
        "ElementRadialGradient  " ++ show grad
    show (ElementGeometry tree) =
        "ElementGeometry " ++ show tree
    show (Pattern ) = "ElementPicture ()"
    -}

data Document = Document
    { _viewBox     :: Maybe (Int, Int, Int, Int)
    , _width       :: Maybe Number
    , _height      :: Maybe Number
    , _elements    :: [Tree]
    , _definitions :: M.Map String Element
    , _description  :: String
    , _styleText   :: String
    , _styleRules  :: [CssRule]
    }
    deriving Show

documentSize :: Document -> (Int, Int)
documentSize Document { _width = Just (Num w)
                            , _height = Just (Num h) } = (floor w, floor h)
documentSize Document { _viewBox = Just (x1, y1, x2, y2)
                            , _width = Just (Percent pw)
                            , _height = Just (Percent ph)
                            } =
    (floor $ dx * pw, floor $ dy * ph)
      where
        dx = fromIntegral . abs $ x2 - x1
        dy = fromIntegral . abs $ y2 - y1
documentSize Document { _viewBox = Just (x1, y1, x2, y2) } =
    (abs $ x2 - x1, abs $ y2 - y1)
documentSize _ = (1, 1)

mayMerge :: Monoid a => Maybe a -> Maybe a -> Maybe a
mayMerge (Just a) (Just b) = Just $ mappend a b
mayMerge _ b@(Just _) = b
mayMerge a Nothing = a

instance Monoid DrawAttributes where
    mempty = DrawAttributes 
        { _strokeWidth      = Last Nothing
        , _strokeColor      = Last Nothing
        , _strokeOpacity    = Nothing
        , _strokeLineCap    = Last Nothing
        , _strokeLineJoin   = Last Nothing
        , _strokeMiterLimit = Last Nothing
        , _fillColor        = Last Nothing
        , _fillOpacity      = Nothing
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

        , _markerStart      = Last Nothing
        , _markerMid        = Last Nothing
        , _markerEnd        = Last Nothing
        }

    mappend a b = DrawAttributes
        { _strokeWidth = (mappend `on` _strokeWidth) a b
        , _strokeColor =  (mappend `on` _strokeColor) a b
        , _strokeLineCap = (mappend `on` _strokeLineCap) a b
        , _strokeOpacity = (opacityMappend `on` _strokeOpacity) a b
        , _strokeLineJoin = (mappend `on` _strokeLineJoin) a b
        , _strokeMiterLimit = (mappend `on` _strokeMiterLimit) a b
        , _fillColor =  (mappend `on` _fillColor) a b
        , _fillOpacity = (opacityMappend `on` _fillOpacity) a b
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
        , _markerStart = (mappend `on` _markerStart) a b
        , _markerMid = (mappend `on` _markerMid) a b
        , _markerEnd = (mappend `on` _markerEnd) a b
        }
      where
        opacityMappend Nothing Nothing = Nothing
        opacityMappend (Just v) Nothing = Just v
        opacityMappend Nothing (Just v) = Just v
        opacityMappend (Just v) (Just v2) = Just $ v * v2


instance CssMatcheable Tree where
  cssAttribOf _ _ = Nothing
  cssClassOf = fmap T.pack . getLast . view (drawAttr . attrClass)
  cssIdOf = fmap T.pack . view (drawAttr . attrId)
  cssNameOf = nameOfTree

