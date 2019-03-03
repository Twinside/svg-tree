{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
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
    , PathCommand( .. )
    , Transformation( .. )
    , ElementRef( .. )
    , CoordinateUnits( .. )

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
    , Dpi

    , WithDefaultSvg( .. )

      -- * Main type
    , Document( .. )
    , HasDocument( .. )
    , documentSize

      -- * Drawing attributes
    , DrawAttributes( .. )
    , HasDrawAttributes( .. )
    , WithDrawAttributes( .. )

      -- * SVG drawing primitives
      -- ** Rectangle
    , Rectangle( .. )
    , HasRectangle( .. )

      -- ** Line
    , Line( .. )
    , HasLine( .. )

      -- ** Polygon
    , Polygon( .. )
    , HasPolygon( .. )

      -- ** Polyline
    , PolyLine( .. )
    , HasPolyLine( .. )

      -- ** Path
    , Path( .. )
    , HasPath( .. )

      -- ** Circle
    , Circle( .. )
    , HasCircle( .. )

      -- ** Ellipse
    , Ellipse( .. )
    , HasEllipse( .. )

      -- ** Mesh (gradient mesh)
    , GradientPathCommand( .. )
    , MeshGradientType( .. )

    , MeshGradient( .. )
    , HasMeshGradient( .. )

    , MeshGradientRow( .. )
    , HasMeshGradientRow( .. )

    , MeshGradientPatch( .. )
    , HasMeshGradientPatch( .. )

      -- ** Image
    , Image( .. )
    , HasImage( .. )

      -- ** Use
    , Use( .. )
    , HasUse( .. )

      -- * Grouping primitives
      -- ** Group
    , Group( .. )
    , HasGroup( .. )

      -- ** Symbol
    , Symbol( .. )
    , groupOfSymbol

      -- * Text related types
      -- ** Text
    , Text( .. )
    , HasText( .. )
    , TextAnchor( .. )
    , textAt

      -- ** Text path
    , TextPath( .. )
    , HasTextPath( .. )

    , TextPathSpacing( .. )
    , TextPathMethod( .. )

      -- ** Text span.
    , TextSpanContent( .. )

    , TextSpan( .. )
    , HasTextSpan( .. )

    , TextInfo( .. )
    , HasTextInfo( .. )

    , TextAdjust( .. )

      -- * Marker definition
    , Marker( .. )
    , Overflow( .. )
    , MarkerOrientation( .. )
    , MarkerUnit( .. )
    , HasMarker( .. )

      -- * Gradient definition
    , GradientStop( .. )
    , HasGradientStop( .. )

      -- ** Linear Gradient
    , LinearGradient( .. )
    , HasLinearGradient( .. )

      -- ** Radial Gradient
    , RadialGradient( .. )
    , HasRadialGradient( .. )

      -- * Pattern definition
    , Pattern( .. )
    , HasPattern( .. )

      -- * Mask definition
    , Mask( .. )
    , HasMask( .. )

      -- * Clip path definition
    , ClipPath( .. )
    , HasClipPath( .. )

      -- * Aspect Ratio description
    , PreserveAspectRatio( .. )
    , Alignment( .. )
    , MeetSlice( .. )
    , HasPreserveAspectRatio( .. )

      -- * MISC functions
    , isPathArc
    , isPathWithArc
    , nameOfTree
    , zipTree
    , mapTree
    , foldTree
    , toUserUnit
    , mapNumber
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( Monoid( .. ) )
import Data.Foldable( Foldable )
#endif

import Data.Function( on )
import Data.List( inits )
import qualified Data.Map as M
import Data.Semigroup( Semigroup( .. ) )
import Data.Monoid( Last( .. ) )
import qualified Data.Foldable as F
import qualified Data.Text as T
import Codec.Picture( PixelRGBA8( .. ) )
import Control.Lens( Lens'
                   , Lens
                   , lens
                   , view
                   , (^.)
                   , (&)
                   , (.~)
                   )
import Graphics.Svg.CssTypes
import Linear hiding ( angle )

import Text.Printf

-- | Basic coordinate type.
type Coord = Double

-- | Real Point, fully determined and not
-- dependant of the rendering context.
type RPoint = V2 Coord

-- | Possibly context dependant point.
type Point = (Number, Number)

-- | Tell if a path command is absolute (in the current
-- user coordiante) or relative to the previous poitn.
data Origin
  = OriginAbsolute -- ^ Next point in absolute coordinate
  | OriginRelative -- ^ Next point relative to the previous
  deriving (Eq, Show)

data MeshGradientType
  = GradientBilinear
  | GradientBicubic
  deriving (Eq, Show)

-- | Path command definition.
data PathCommand
      -- | 'M' or 'm' command
    = MoveTo !Origin ![RPoint]
      -- | Line to, 'L' or 'l' Svg path command.
    | LineTo !Origin ![RPoint]

      -- | Equivalent to the 'H' or 'h' svg path command.
    | HorizontalTo  !Origin ![Coord]
      -- | Equivalent to the 'V' or 'v' svg path command.
    | VerticalTo    !Origin ![Coord]

    -- | Cubic bezier, 'C' or 'c' command
    | CurveTo  !Origin ![(RPoint, RPoint, RPoint)]
    -- | Smooth cubic bezier, equivalent to 'S' or 's' command
    | SmoothCurveTo  !Origin ![(RPoint, RPoint)]
    -- | Quadratic bezier, 'Q' or 'q' command
    | QuadraticBezier !Origin ![(RPoint, RPoint)]
    -- | Quadratic bezier, 'T' or 't' command
    | SmoothQuadraticBezierCurveTo  !Origin ![RPoint]
      -- | Eliptical arc, 'A' or 'a' command.
    | EllipticalArc !Origin ![(Coord, Coord, Coord, Bool, Bool, RPoint)]
      -- | Close the path, 'Z' or 'z' svg path command.
    | EndPath
    deriving (Eq, Show)

-- | Description of path used in meshgradient tag
data GradientPathCommand
      -- | Line to, 'L' or 'l' Svg path command.
    = GLine !Origin !(Maybe RPoint)
      -- | Cubic bezier, 'C' or 'c' command
    | GCurve !Origin !RPoint !RPoint !(Maybe RPoint)
      -- | 'Z' command
    | GClose
    deriving (Eq, Show)

-- | Little helper function to build a point.
toPoint :: Number -> Number -> Point
toPoint = (,)

-- | Tell if the path command is an EllipticalArc.
isPathArc :: PathCommand -> Bool
isPathArc (EllipticalArc _ _) = True
isPathArc _ = False

-- | Tell if a full path contain an EllipticalArc.
isPathWithArc :: Foldable f => f PathCommand -> Bool
isPathWithArc = F.any isPathArc

-- | Define the possible values of various *units attributes
-- used in the definition of the gradients and masks.
data CoordinateUnits
    = CoordUserSpace   -- ^ `userSpaceOnUse` value
    | CoordBoundingBox -- ^ `objectBoundingBox` value
    deriving (Eq, Show)

-- | This type represent the align information of the
-- preserveAspectRatio SVGattribute
data Alignment
  = AlignNone -- ^ "none" value
  | AlignxMinYMin -- "xMinYMin" value
  | AlignxMidYMin -- ^ "xMidYMin" value
  | AlignxMaxYMin -- ^ "xMaxYMin" value
  | AlignxMinYMid -- ^ "xMinYMid" value
  | AlignxMidYMid -- ^ "xMidYMid" value
  | AlignxMaxYMid -- ^ "xMaxYMid" value
  | AlignxMinYMax -- ^ "xMinYMax" value
  | AlignxMidYMax -- ^ "xMidYMax" value
  | AlignxMaxYMax -- ^ "xMaxYMax" value
  deriving (Eq, Show)

-- | This type represent the "meet or slice" information
-- of the preserveAspectRatio SVGattribute
data MeetSlice = Meet | Slice
    deriving (Eq, Show)

-- | Describe the content of the preserveAspectRatio attribute.
data PreserveAspectRatio = PreserveAspectRatio
  { _aspectRatioDefer     :: !Bool
  , _aspectRatioAlign     :: !Alignment
  , _aspectRatioMeetSlice :: !(Maybe MeetSlice)
  }
  deriving (Eq, Show)

instance WithDefaultSvg PreserveAspectRatio where
  defaultSvg = PreserveAspectRatio
    { _aspectRatioDefer     = False
    , _aspectRatioAlign     = AlignxMidYMid
    , _aspectRatioMeetSlice = Nothing
    }

-- | Describe how the line should be terminated
-- when stroking them. Describe the values of the
-- `stroke-linecap` attribute.
-- See `_strokeLineCap`
data Cap
  = CapRound -- ^ End with a round (`round` value)
  | CapButt  -- ^ Define straight just at the end (`butt` value)
  | CapSquare -- ^ Straight further of the ends (`square` value)
  deriving (Eq, Show)

-- | Define the possible values of the `stroke-linejoin`
-- attribute.
-- see `_strokeLineJoin`
data LineJoin
    = JoinMiter -- ^ `miter` value
    | JoinBevel -- ^ `bevel` value
    | JoinRound -- ^ `round` value
    deriving (Eq, Show)

-- | Describe the different value which can be used
-- in the `fill` or `stroke` attributes.
data Texture
  = ColorRef   PixelRGBA8 -- ^ Direct solid color (#rrggbb, #rgb)
  | TextureRef String     -- ^ Link to a complex texture (url(#name))
  | FillNone              -- ^ Equivalent to the `none` value.
  deriving (Eq, Show)

-- | Describe the possile filling algorithms.
-- Map the values of the `fill-rule` attributes.
data FillRule
    = FillEvenOdd -- ^ Correspond to the `evenodd` value.
    | FillNonZero -- ^ Correspond to the `nonzero` value.
    deriving (Eq, Show)

-- | Describe the content of the `transformation` attribute.
-- see `_transform` and `transform`.
data Transformation
    = -- | Directly encode the translation matrix.
      TransformMatrix !Coord !Coord !Coord
                      !Coord !Coord !Coord
      -- | Translation along a vector
    | Translate !Double !Double
      -- | Scaling on both axis or on X axis and Y axis.
    | Scale !Double !(Maybe Double)
      -- | Rotation around `(0, 0)` or around an optional
      -- point.
    | Rotate !Double !(Maybe (Double, Double))
      -- | Skew transformation along the X axis.
    | SkewX !Double
      -- | Skew transformation along the Y axis.
    | SkewY !Double
      -- | Unkown transformation, like identity.
    | TransformUnknown
    deriving (Eq, Show)

-- | Convert the Transformation to a string which can be
-- directly used in a svg attributes.
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

-- | Transform a list of transformations to a string for svg
-- `transform` attributes.
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
    -- Equivalent to the `start` value.
  = TextAnchorStart
    -- | The text is middle aligned, so the text will be at
    -- the left and right of the position:
    --
    -- >   THE_TEXT*TO_PRINT
    --
    -- Equivalent to the `middle` value.
  | TextAnchorMiddle
    -- | The text is right aligned.
    --
    -- >   THE_TEXT_TO_PRINT*
    --
    -- Equivalent to the `end` value.
  | TextAnchorEnd
  deriving (Eq, Show)


-- | Correspond to the possible values of the
-- the attributes which are either `none` or
-- `url(#elem)`
data ElementRef
  = RefNone  -- ^ Value for `none`
  | Ref String -- ^ Equivalent to `url()` attribute.
  deriving (Eq, Show)

-- | This type define how to draw any primitives,
-- which color to use, how to stroke the primitives
-- and the potential transformations to use.
--
-- All these attributes are propagated to the children.
data DrawAttributes = DrawAttributes
    { -- | Attribute corresponding to the `stroke-width`
      -- SVG attribute.
      _strokeWidth      :: !(Last Number)
      -- | Correspond to the `stroke` attribute.
    , _strokeColor      :: !(Last Texture)
      -- | Define the `stroke-opacity` attribute, the transparency
      -- for the "border".
    , _strokeOpacity    :: !(Maybe Float)
      -- | Correspond to the `stroke-linecap` SVG
      -- attribute
    , _strokeLineCap    :: !(Last Cap)
      -- | Correspond to the `stroke-linejoin` SVG
      -- attribute
    , _strokeLineJoin   :: !(Last LineJoin)
      -- | Define the distance of the miter join, correspond
      -- to the `stroke-miterlimit` attritbue.
    , _strokeMiterLimit :: !(Last Double)
      -- | Define the filling color of the elements. Corresponding
      -- to the `fill` attribute.
    , _fillColor        :: !(Last Texture)
      -- | Define the `fill-opacity` attribute, the transparency
      -- for the "content".
    , _fillOpacity      :: !(Maybe Float)
      -- | Define the global or group opacity attribute.
    , _groupOpacity     :: !(Maybe Float)
      -- | Content of the `transform` attribute
    , _transform        :: !(Maybe [Transformation])
      -- | Define the `fill-rule` used during the rendering.
    , _fillRule         :: !(Last FillRule)
      -- | Define the `mask` attribute.
    , _maskRef          :: !(Last ElementRef)
      -- | Define the `clip-path` attribute.
    , _clipPathRef      :: !(Last ElementRef)
      -- | Define the `clip-rule` attribute.
    , _clipRule         :: !(Last FillRule)
      -- | Map to the `class` attribute. Used for the CSS
      -- rewriting.
    , _attrClass        :: ![T.Text]
      -- | Map to the `id` attribute. Used for the CSS
      -- rewriting.
    , _attrId           :: !(Maybe String)
      -- | Define the start distance of the dashing pattern.
      -- Correspond to the `stroke-dashoffset` attribute.
    , _strokeOffset     :: !(Last Number)
      -- | Define the dashing pattern for the lines. Correspond
      -- to the `stroke-dasharray` attribute.
    , _strokeDashArray  :: !(Last [Number])
      -- | Current size of the text, correspond to the
      -- `font-size` SVG attribute.
    , _fontSize         :: !(Last Number)
      -- | Define the possible fonts to be used for text rendering.
      -- Map to the `font-family` attribute.
    , _fontFamily       :: !(Last [String])
      -- | Map to the `font-style` attribute.
    , _fontStyle        :: !(Last FontStyle)
      -- | Define how to interpret the text position, correspond
      -- to the `text-anchor` attribute.
    , _textAnchor       :: !(Last TextAnchor)
      -- | Define the marker used for the start of the line.
      -- Correspond to the `marker-start` attribute.
    , _markerStart      :: !(Last ElementRef)
      -- | Define the marker used for every point of the
      -- polyline/path Correspond to the `marker-mid`
      -- attribute.
    , _markerMid        :: !(Last ElementRef)
      -- | Define the marker used for the end of the line.
      -- Correspond to the `marker-end` attribute.
    , _markerEnd        :: !(Last ElementRef)
    }
    deriving (Eq, Show)


-- | This primitive describe an unclosed suite of
-- segments. Correspond to the `<polyline>` tag.
data PolyLine = PolyLine
  { -- | drawing attributes of the polyline.
    _polyLineDrawAttributes :: !DrawAttributes

    -- | Geometry definition of the polyline.
    -- correspond to the `points` attribute
  , _polyLinePoints :: ![RPoint]
  }
  deriving (Eq, Show)


instance WithDefaultSvg PolyLine where
  defaultSvg = PolyLine
    { _polyLineDrawAttributes = mempty
    , _polyLinePoints = []
    }

-- makeClassy ''PolyLine
-- | Lenses for the PolyLine type.
class HasPolyLine a where
  polyLine :: Lens' a PolyLine
  polyLineDrawAttributes :: Lens' a DrawAttributes
  {-# INLINE polyLineDrawAttributes #-}
  polyLineDrawAttributes = polyLine . polyLineDrawAttributes

  polyLinePoints :: Lens' a [RPoint]
  {-# INLINE polyLinePoints #-}
  polyLinePoints = polyLine . polyLinePoints

instance HasPolyLine PolyLine where
  polyLine = id
  {-# INLINE polyLineDrawAttributes #-}
  polyLineDrawAttributes f p =
    fmap (\y -> p { _polyLineDrawAttributes = y }) (f $ _polyLineDrawAttributes p)
  {-# INLINE polyLinePoints #-}
  polyLinePoints f p =
    fmap (\y -> p { _polyLinePoints = y }) (f $ _polyLinePoints p)

instance WithDrawAttributes PolyLine where
    drawAttr = polyLineDrawAttributes

-- | Primitive decriving polygon composed
-- of segements. Correspond to the `<polygon>`
-- tag
data Polygon = Polygon
  { -- | Drawing attributes for the polygon.
    _polygonDrawAttributes :: !DrawAttributes
    -- | Points of the polygon. Correspond to
    -- the `points` attributes.
  , _polygonPoints :: ![RPoint]
  }
  deriving (Eq, Show)

-- makeClassy ''Polygon
-- | Lenses for the Polygon type
class HasPolygon a where
  polygon :: Lens' a Polygon
  polygonDrawAttributes :: Lens' a DrawAttributes
  {-# INLINE polygonDrawAttributes #-}
  polygonPoints :: Lens' a [RPoint]
  {-# INLINE polygonPoints #-}
  polygonDrawAttributes = polygon . polygonDrawAttributes
  polygonPoints = polygon . polygonPoints

instance HasPolygon Polygon where
  polygon = id
  {-# INLINE polygonDrawAttributes #-}
  polygonDrawAttributes f p =
    fmap (\y -> p { _polygonDrawAttributes = y }) (f $ _polygonDrawAttributes p)
  {-# INLINE polygonPoints #-}
  polygonPoints f p =
    fmap (\y -> p { _polygonPoints = y }) (f $ _polygonPoints p)

instance WithDrawAttributes Polygon where
    drawAttr = polygonDrawAttributes

instance WithDefaultSvg Polygon where
  defaultSvg = Polygon
    { _polygonDrawAttributes = mempty
    , _polygonPoints = []
    }

-- | Define a simple line. Correspond to the
-- `<line>` tag.
data Line = Line
  { -- | Drawing attributes of line.
    _lineDrawAttributes :: !DrawAttributes
    -- | First point of the the line, correspond
    -- to the `x1` and `y1` attributes.
  , _linePoint1 :: !Point
    -- | Second point of the the line, correspond
    -- to the `x2` and `y2` attributes.
  , _linePoint2 :: !Point
  }
  deriving (Eq, Show)

-- makeClassy ''Line
-- | Lenses for the Line type.
class HasLine a where
  line :: Lens' a Line
  lineDrawAttributes :: Lens' a DrawAttributes
  lineDrawAttributes = line . lineDrawAttributes
  {-# INLINE lineDrawAttributes #-}
  linePoint1 :: Lens' a Point
  linePoint1 = line . linePoint1
  {-# INLINE linePoint1 #-}
  linePoint2 :: Lens' a Point
  linePoint2 = line . linePoint2
  {-# INLINE linePoint2 #-}

instance HasLine Line where
  line = id
  {-# INLINE lineDrawAttributes #-}
  lineDrawAttributes f l =
      fmap (\y -> l { _lineDrawAttributes = y }) (f (_lineDrawAttributes l))
  {-# INLINE linePoint1 #-}
  linePoint1 f l =
      fmap (\y -> l { _linePoint1 = y }) (f (_linePoint1 l))
  {-# INLINE linePoint2 #-}
  linePoint2 f l =
      fmap (\y -> l { _linePoint2 = y }) (f (_linePoint2 l))

instance WithDrawAttributes Line where
    drawAttr = lineDrawAttributes

instance WithDefaultSvg Line where
  defaultSvg = Line
    { _lineDrawAttributes = mempty
    , _linePoint1 = zeroPoint
    , _linePoint2 = zeroPoint
    }
    where zeroPoint = (Num 0, Num 0)

-- | Define a rectangle. Correspond to
-- `<rectangle>` svg tag.
data Rectangle = Rectangle
  { -- | Rectangle drawing attributes.
    _rectDrawAttributes  :: !DrawAttributes
    -- | Upper left corner of the rectangle, correspond
    -- to the attributes `x` and `y`.
  , _rectUpperLeftCorner :: !Point
    -- | Rectangle width, correspond, strangely, to
    -- the `width` attribute.
  , _rectWidth           :: !(Maybe Number)
    -- | Rectangle height, correspond, amazingly, to
    -- the `height` attribute.
  , _rectHeight          :: !(Maybe Number)
    -- | Define the rounded corner radius radius
    -- of the rectangle. Correspond to the `rx` and
    -- `ry` attributes.
  , _rectCornerRadius    :: !(Maybe Number, Maybe Number)
  }
  deriving (Eq, Show)

-- makeClassy ''Rectangle
-- | Lenses for the Rectangle type.
class HasRectangle a where
  rectangle :: Lens' a Rectangle
  rectCornerRadius :: Lens' a (Maybe Number, Maybe Number)
  {-# INLINE rectCornerRadius #-}
  rectCornerRadius = rectangle . rectCornerRadius

  rectDrawAttributes :: Lens' a DrawAttributes
  {-# INLINE rectDrawAttributes #-}
  rectDrawAttributes = rectangle . rectDrawAttributes

  rectHeight :: Lens' a (Maybe Number)
  {-# INLINE rectHeight #-}
  rectHeight = rectangle . rectHeight

  rectUpperLeftCorner :: Lens' a Point
  {-# INLINE rectUpperLeftCorner #-}
  rectUpperLeftCorner = rectangle . rectUpperLeftCorner

  rectWidth :: Lens' a (Maybe Number)
  {-# INLINE rectWidth #-}
  rectWidth = rectangle . rectWidth

instance HasRectangle Rectangle where
  rectangle = id
  {-# INLINE rectCornerRadius #-}
  rectCornerRadius f attr =
    fmap (\y -> attr { _rectCornerRadius = y }) (f $ _rectCornerRadius attr)
  {-# INLINE rectDrawAttributes #-}
  rectDrawAttributes f attr =
    fmap (\y -> attr { _rectDrawAttributes = y }) (f $ _rectDrawAttributes attr)
  {-# INLINE rectHeight #-}
  rectHeight f attr =
    fmap (\y -> attr { _rectHeight = y }) (f $ _rectHeight attr)
  {-# INLINE rectUpperLeftCorner #-}
  rectUpperLeftCorner f attr =
    fmap (\y -> attr { _rectUpperLeftCorner = y }) (f $ _rectUpperLeftCorner attr)
  {-# INLINE rectWidth #-}
  rectWidth f attr =
    fmap (\y -> attr { _rectWidth = y }) (f $ _rectWidth attr)

instance WithDrawAttributes Rectangle where
    drawAttr = rectDrawAttributes

instance WithDefaultSvg Rectangle where
  defaultSvg = Rectangle
    { _rectDrawAttributes  = mempty
    , _rectUpperLeftCorner = (Num 0, Num 0)
    , _rectWidth           = Nothing
    , _rectHeight          = Nothing
    , _rectCornerRadius    = (Nothing, Nothing)
    }

-- | Type mapping the `<path>` svg tag.
data Path = Path
  { -- | Drawing attributes of the path.
    _pathDrawAttributes :: !DrawAttributes
    -- | Definition of the path, correspond to the
    -- `d` attributes.
  , _pathDefinition :: ![PathCommand]
  }
  deriving (Eq, Show)

-- makeClassy ''Path
-- | Lenses for the Path type
class HasPath c_alhy where
  path :: Lens' c_alhy Path
  pathDefinition :: Lens' c_alhy [PathCommand]
  {-# INLINE pathDefinition #-}
  pathDefinition = path . pathDefinition

  pathDrawAttributes :: Lens' c_alhy DrawAttributes
  {-# INLINE pathDrawAttributes #-}
  pathDrawAttributes = path . pathDrawAttributes

instance HasPath Path where
  path = id
  {-# INLINE pathDefinition #-}
  pathDefinition f attr =
    fmap (\y -> attr { _pathDefinition = y }) (f $ _pathDefinition attr)
  {-# INLINE pathDrawAttributes #-}
  pathDrawAttributes f attr =
    fmap (\y -> attr { _pathDrawAttributes = y }) (f $ _pathDrawAttributes attr)

instance WithDrawAttributes Path where
  drawAttr = pathDrawAttributes

instance WithDefaultSvg Path where
  defaultSvg = Path
    { _pathDrawAttributes = mempty
    , _pathDefinition = []
    }

-- | Define a SVG group, corresponding `<g>` tag.
data Group a = Group
  { -- | Group drawing attributes, propagated to all of its
    -- children.
    _groupDrawAttributes :: !DrawAttributes
    -- | Content of the group, corresponding to all the tags
    -- inside the `<g>` tag.
  , _groupChildren  :: ![a]
    -- | Mapped to the attribute `viewBox`
  , _groupViewBox   :: !(Maybe (Double, Double, Double, Double))
    -- | used for symbols only
  , _groupAspectRatio :: !PreserveAspectRatio
  }
  deriving (Eq, Show)

-- makeClassy ''Group
-- | Lenses associated to the Group type.
class HasGroup g a | g -> a where
  group :: Lens' g (Group a)
  groupAspectRatio :: Lens' g PreserveAspectRatio
  {-# INLINE groupAspectRatio #-}
  groupAspectRatio = group . groupAspectRatio

  groupChildren :: Lens' g [a]
  {-# INLINE groupChildren #-}
  groupChildren = group . groupChildren

  groupDrawAttributes :: Lens' g DrawAttributes
  {-# INLINE groupDrawAttributes #-}
  groupDrawAttributes = group . groupDrawAttributes

  groupViewBox :: Lens' g (Maybe (Double, Double, Double, Double))
  {-# INLINE groupViewBox #-}
  groupViewBox = group . groupViewBox

instance HasGroup (Group a) a where
  group = id
  {-# INLINE groupAspectRatio #-}
  groupAspectRatio f attr =
    fmap (\y -> attr { _groupAspectRatio = y }) (f $ _groupAspectRatio attr)

  {-# INLINE groupChildren #-}
  groupChildren f attr =
    fmap (\y -> attr { _groupChildren = y }) (f $ _groupChildren attr)

  {-# INLINE groupDrawAttributes #-}
  groupDrawAttributes f attr =
    fmap (\y -> attr { _groupDrawAttributes = y }) (f $ _groupDrawAttributes attr)

  {-# INLINE groupViewBox #-}
  groupViewBox f attr =
    fmap (\y -> attr { _groupViewBox = y }) (f $ _groupViewBox attr)

instance WithDrawAttributes (Group a) where
    drawAttr = groupDrawAttributes

instance WithDefaultSvg (Group a) where
  defaultSvg = Group
    { _groupDrawAttributes = mempty
    , _groupChildren  = []
    , _groupViewBox = Nothing
    , _groupAspectRatio = defaultSvg
    }

-- | Define the `<symbol>` tag, equivalent to
-- a named group.
newtype Symbol a =
    Symbol { _groupOfSymbol :: Group a }
  deriving (Eq, Show)

-- makeLenses ''Symbol
-- | Lenses associated with the Symbol type.
groupOfSymbol :: Lens (Symbol s) (Symbol t) (Group s) (Group t)
{-# INLINE groupOfSymbol #-}
groupOfSymbol f = fmap Symbol . f . _groupOfSymbol

instance WithDrawAttributes (Symbol a) where
  drawAttr = groupOfSymbol . drawAttr

instance WithDefaultSvg (Symbol a) where
  defaultSvg = Symbol defaultSvg

-- | Define a `<circle>`.
data Circle = Circle
  { -- | Drawing attributes of the circle.
    _circleDrawAttributes :: !DrawAttributes
    -- | Define the center of the circle, describe
    -- the `cx` and `cy` attributes.
  , _circleCenter   :: !Point
    -- | Radius of the circle, equivalent to the `r`
    -- attribute.
  , _circleRadius   :: !Number
  }
  deriving (Eq, Show)

-- makeClassy ''Circle
-- | Lenses for the Circle type.
class HasCircle a where
  circle :: Lens' a Circle
  circleCenter :: Lens' a Point
  {-# INLINE circleCenter #-}
  circleCenter = circle . circleCenter

  circleDrawAttributes :: Lens' a DrawAttributes
  {-# INLINE circleDrawAttributes #-}
  circleDrawAttributes = circle . circleDrawAttributes

  circleRadius :: Lens' a Number
  {-# INLINE circleRadius #-}
  circleRadius = circle . circleRadius

instance HasCircle Circle where
  circle = id
  {-# INLINE circleCenter #-}
  circleCenter f attr =
    fmap (\y -> attr { _circleCenter = y }) (f $ _circleCenter attr)
  {-# INLINE circleDrawAttributes #-}
  circleDrawAttributes f attr =
    fmap (\y -> attr { _circleDrawAttributes = y }) (f $ _circleDrawAttributes attr)
  {-# INLINE circleRadius #-}
  circleRadius f attr =
    fmap (\y -> attr { _circleRadius = y }) (f $ _circleRadius attr)

instance WithDrawAttributes Circle where
    drawAttr = circleDrawAttributes

instance WithDefaultSvg Circle where
  defaultSvg = Circle
    { _circleDrawAttributes = mempty
    , _circleCenter = (Num 0, Num 0)
    , _circleRadius = Num 0
    }

-- | Define an `<ellipse>`
data Ellipse = Ellipse
  {  -- | Drawing attributes of the ellipse.
    _ellipseDrawAttributes :: !DrawAttributes
    -- | Center of the ellipse, map to the `cx`
    -- and `cy` attributes.
  , _ellipseCenter :: !Point
    -- | Radius along the X axis, map the
    -- `rx` attribute.
  , _ellipseXRadius :: !Number
    -- | Radius along the Y axis, map the
    -- `ry` attribute.
  , _ellipseYRadius :: !Number
  }
  deriving (Eq, Show)

-- makeClassy ''Ellipse
-- | Lenses for the ellipse type.
class HasEllipse c_amWt where
  ellipse :: Lens' c_amWt Ellipse
  ellipseCenter :: Lens' c_amWt Point
  {-# INLINE ellipseCenter #-}
  ellipseDrawAttributes :: Lens' c_amWt DrawAttributes
  {-# INLINE ellipseDrawAttributes #-}
  ellipseXRadius :: Lens' c_amWt Number
  {-# INLINE ellipseXRadius #-}
  ellipseYRadius :: Lens' c_amWt Number
  {-# INLINE ellipseYRadius #-}
  ellipseCenter = ((.) ellipse) ellipseCenter
  ellipseDrawAttributes = ((.) ellipse) ellipseDrawAttributes
  ellipseXRadius = ((.) ellipse) ellipseXRadius
  ellipseYRadius = ((.) ellipse) ellipseYRadius

instance HasEllipse Ellipse where
  {-# INLINE ellipseCenter #-}
  {-# INLINE ellipseDrawAttributes #-}
  {-# INLINE ellipseXRadius #-}
  {-# INLINE ellipseYRadius #-}
  ellipse = id
  ellipseCenter f attr =
    fmap (\y -> attr { _ellipseCenter = y }) (f $ _ellipseCenter attr)
  ellipseDrawAttributes f attr =
    fmap (\y -> attr { _ellipseDrawAttributes = y }) (f $ _ellipseDrawAttributes attr)
  ellipseXRadius f attr =
    fmap (\y -> attr { _ellipseXRadius = y }) (f $ _ellipseXRadius attr)
  ellipseYRadius f attr =
    fmap (\y -> attr { _ellipseYRadius = y }) (f $ _ellipseYRadius attr)

instance WithDrawAttributes Ellipse where
  drawAttr = ellipseDrawAttributes

instance WithDefaultSvg Ellipse where
  defaultSvg = Ellipse
    { _ellipseDrawAttributes = mempty
    , _ellipseCenter = (Num 0, Num 0)
    , _ellipseXRadius = Num 0
    , _ellipseYRadius = Num 0
    }

-- | Define a color stop for the gradients. Represent
-- the `<stop>` SVG tag.
data GradientStop = GradientStop
    { -- | Gradient offset between 0 and 1, correspond
      -- to the `offset` attribute.
      _gradientOffset :: !Float
      -- | Color of the gradient stop. Correspond
      -- to the `stop-color` attribute.
    , _gradientColor  :: !PixelRGBA8
      -- | Path command used in mesh patch
    , _gradientPath   :: !(Maybe GradientPathCommand)
      -- | Stop color opacity
    , _gradientOpacity :: !(Maybe Float)
    }
    deriving (Eq, Show)

-- makeClassy ''GradientStop
-- | Lenses for the GradientStop type.
class HasGradientStop c_anhM where
  gradientStop :: Lens' c_anhM GradientStop
  gradientColor :: Lens' c_anhM PixelRGBA8
  {-# INLINE gradientColor #-}
  gradientOffset :: Lens' c_anhM Float
  {-# INLINE gradientOffset #-}
  gradientOpacity :: Lens' c_anhM (Maybe Float)
  {-# INLINE gradientOpacity #-}
  gradientPath :: Lens' c_anhM (Maybe GradientPathCommand)
  {-# INLINE gradientPath #-}
  gradientColor = ((.) gradientStop) gradientColor
  gradientOffset = ((.) gradientStop) gradientOffset
  gradientOpacity = ((.) gradientStop) gradientOpacity
  gradientPath = ((.) gradientStop) gradientPath

instance HasGradientStop GradientStop where
  {-# INLINE gradientColor #-}
  {-# INLINE gradientOffset #-}
  {-# INLINE gradientOpacity #-}
  {-# INLINE gradientPath #-}
  gradientStop = id
  gradientColor f attr =
    fmap (\y -> attr { _gradientColor = y }) (f $ _gradientColor attr)
  gradientOffset f attr =
    fmap (\y -> attr { _gradientOffset = y }) (f $ _gradientOffset attr)
  gradientOpacity f attr =
    fmap (\y -> attr { _gradientOpacity = y }) (f $ _gradientOpacity attr)
  gradientPath f attr =
    fmap (\y -> attr { _gradientPath = y }) (f $ _gradientPath attr)

instance WithDefaultSvg GradientStop where
  defaultSvg = GradientStop
    { _gradientOffset = 0.0
    , _gradientColor  = PixelRGBA8 0 0 0 255
    , _gradientPath   = Nothing
    , _gradientOpacity = Nothing
    }


-- | Define `<meshpatch>` SVG tag
data MeshGradientPatch = MeshGradientPatch
  { -- | List of stop, from 2 to 4 in a patch
    _meshGradientPatchStops :: ![GradientStop]
  }
  deriving (Eq, Show)

-- makeClassy ''MeshGradientPatch
class HasMeshGradientPatch c_annx where
  meshGradientPatch :: Lens' c_annx MeshGradientPatch
  meshGradientPatchStops :: Lens' c_annx [GradientStop]
  {-# INLINE meshGradientPatchStops #-}
  meshGradientPatchStops =  meshGradientPatch . meshGradientPatchStops

instance HasMeshGradientPatch MeshGradientPatch where
  {-# INLINE meshGradientPatchStops #-}
  meshGradientPatch = id
  meshGradientPatchStops f m =
    fmap (\y -> m { _meshGradientPatchStops = y }) . f $ _meshGradientPatchStops m

instance WithDefaultSvg MeshGradientPatch where
  defaultSvg = MeshGradientPatch []

-- | Define a `<meshrow>` tag.
data MeshGradientRow = MeshGradientRow
  { -- | List of patch in a row
    _meshGradientRowPatches :: ![MeshGradientPatch]
  }
  deriving (Eq, Show)

-- makeClassy ''MeshGradientRow
class HasMeshGradientRow c_antr where
  meshGradientRow :: Lens' c_antr MeshGradientRow
  meshGradientRowPatches :: Lens' c_antr [MeshGradientPatch]
  {-# INLINE meshGradientRowPatches #-}
  meshGradientRowPatches = meshGradientRow . meshGradientRowPatches

instance HasMeshGradientRow MeshGradientRow where
  {-# INLINE meshGradientRowPatches #-}
  meshGradientRow = id
  meshGradientRowPatches f m =
      fmap (\y -> m { _meshGradientRowPatches = y }) . f $ _meshGradientRowPatches m

instance WithDefaultSvg MeshGradientRow where
  defaultSvg = MeshGradientRow []


-- | Define a `<meshgradient>` tag.
data MeshGradient = MeshGradient
  { _meshGradientDrawAttributes :: !DrawAttributes
    -- | Original x coordinate of the mesh gradient
  , _meshGradientX              :: !Number
    -- | Original y coordinate of the mesh gradient
  , _meshGradientY              :: !Number
    -- | Type of color interpolation to use
  , _meshGradientType           :: !MeshGradientType
    -- | Coordiante system to use
  , _meshGradientUnits          :: !CoordinateUnits
    -- | Optional transform
  , _meshGradientTransform      :: ![Transformation]
    -- | List of patch rows in the the mesh.
  , _meshGradientRows           :: ![MeshGradientRow]
  }
  deriving (Eq, Show)

-- makeClassy ''MeshGradient
class HasMeshGradient c_anxG where
  meshGradient :: Lens' c_anxG MeshGradient
  meshGradientDrawAttributes :: Lens' c_anxG DrawAttributes
  {-# INLINE meshGradientDrawAttributes #-}
  meshGradientRows :: Lens' c_anxG [MeshGradientRow]
  {-# INLINE meshGradientRows #-}
  meshGradientTransform :: Lens' c_anxG [Transformation]
  {-# INLINE meshGradientTransform #-}
  meshGradientType :: Lens' c_anxG MeshGradientType
  {-# INLINE meshGradientType #-}
  meshGradientUnits :: Lens' c_anxG CoordinateUnits
  {-# INLINE meshGradientUnits #-}
  meshGradientX :: Lens' c_anxG Number
  {-# INLINE meshGradientX #-}
  meshGradientY :: Lens' c_anxG Number
  {-# INLINE meshGradientY #-}
  meshGradientDrawAttributes
    = ((.) meshGradient) meshGradientDrawAttributes
  meshGradientRows = ((.) meshGradient) meshGradientRows
  meshGradientTransform = ((.) meshGradient) meshGradientTransform
  meshGradientType = ((.) meshGradient) meshGradientType
  meshGradientUnits = ((.) meshGradient) meshGradientUnits
  meshGradientX = ((.) meshGradient) meshGradientX
  meshGradientY = ((.) meshGradient) meshGradientY
instance HasMeshGradient MeshGradient where
  {-# INLINE meshGradientDrawAttributes #-}
  {-# INLINE meshGradientRows #-}
  {-# INLINE meshGradientTransform #-}
  {-# INLINE meshGradientType #-}
  {-# INLINE meshGradientUnits #-}
  {-# INLINE meshGradientX #-}
  {-# INLINE meshGradientY #-}
  meshGradient = id
  meshGradientDrawAttributes f attr =
    fmap (\y -> attr { _meshGradientDrawAttributes = y }) (f $ _meshGradientDrawAttributes attr)
  meshGradientRows f attr =
    fmap (\y -> attr { _meshGradientRows = y }) (f $ _meshGradientRows attr)
  meshGradientTransform f attr =
    fmap (\y -> attr { _meshGradientTransform = y }) (f $ _meshGradientTransform attr)
  meshGradientType f attr =
    fmap (\y -> attr { _meshGradientType = y }) (f $ _meshGradientType attr)
  meshGradientUnits f attr =
    fmap (\y -> attr { _meshGradientUnits = y }) (f $ _meshGradientUnits attr)
  meshGradientX f attr =
    fmap (\y -> attr { _meshGradientX = y }) (f $ _meshGradientX attr)
  meshGradientY f attr =
    fmap (\y -> attr { _meshGradientY = y }) (f $ _meshGradientY attr)

instance WithDrawAttributes MeshGradient where
  drawAttr = meshGradientDrawAttributes

instance WithDefaultSvg MeshGradient where
  defaultSvg = MeshGradient
    { _meshGradientDrawAttributes = mempty
    , _meshGradientX              = Percent 0
    , _meshGradientY              = Percent 0
    , _meshGradientType           = GradientBilinear
    , _meshGradientUnits          = CoordBoundingBox
    , _meshGradientTransform      = mempty
    , _meshGradientRows           = mempty
    }


-- | Define an `<image>` tag.
data Image = Image
  { -- | Drawing attributes of the image
    _imageDrawAttributes :: !DrawAttributes
    -- | Position of the image referenced by its
    -- upper left corner.
  , _imageCornerUpperLeft :: !Point
    -- | Image width
  , _imageWidth :: !Number
    -- | Image Height
  , _imageHeight :: !Number
    -- | Image href, pointing to the real image.
  , _imageHref :: !String
    -- | preserveAspectRatio attribute
  , _imageAspectRatio :: !PreserveAspectRatio
  }
  deriving (Eq, Show)

-- makeClassy ''Image
-- | Lenses for the Image type.
class HasImage c_anI7 where
  image :: Lens' c_anI7 Image
  imageAspectRatio :: Lens' c_anI7 PreserveAspectRatio
  {-# INLINE imageAspectRatio #-}
  imageCornerUpperLeft :: Lens' c_anI7 Point
  {-# INLINE imageCornerUpperLeft #-}
  imageDrawAttributes :: Lens' c_anI7 DrawAttributes
  {-# INLINE imageDrawAttributes #-}
  imageHeight :: Lens' c_anI7 Number
  {-# INLINE imageHeight #-}
  imageHref :: Lens' c_anI7 String
  {-# INLINE imageHref #-}
  imageWidth :: Lens' c_anI7 Number
  {-# INLINE imageWidth #-}
  imageAspectRatio = ((.) image) imageAspectRatio
  imageCornerUpperLeft = ((.) image) imageCornerUpperLeft
  imageDrawAttributes = ((.) image) imageDrawAttributes
  imageHeight = ((.) image) imageHeight
  imageHref = ((.) image) imageHref
  imageWidth = ((.) image) imageWidth
instance HasImage Image where
  {-# INLINE imageAspectRatio #-}
  {-# INLINE imageCornerUpperLeft #-}
  {-# INLINE imageDrawAttributes #-}
  {-# INLINE imageHeight #-}
  {-# INLINE imageHref #-}
  {-# INLINE imageWidth #-}
  image = id
  imageAspectRatio f attr =
    fmap (\y -> attr { _imageAspectRatio = y }) (f $ _imageAspectRatio attr)
  imageCornerUpperLeft f attr =
    fmap (\y -> attr { _imageCornerUpperLeft = y }) (f $ _imageCornerUpperLeft attr)
  imageDrawAttributes f attr =
    fmap (\y -> attr { _imageDrawAttributes = y }) (f $ _imageDrawAttributes attr)
  imageHeight f attr =
    fmap (\y -> attr { _imageHeight = y }) (f $ _imageHeight attr)
  imageHref f attr =
    fmap (\y -> attr { _imageHref = y }) (f $ _imageHref attr)
  imageWidth f attr =
    fmap (\y -> attr { _imageWidth = y }) (f $ _imageWidth attr)

instance WithDrawAttributes Image where
  drawAttr = imageDrawAttributes

instance WithDefaultSvg Image where
  defaultSvg = Image
    { _imageDrawAttributes = mempty
    , _imageCornerUpperLeft = (Num 0, Num 0)
    , _imageWidth = Num 0
    , _imageHeight = Num 0
    , _imageHref = ""
    , _imageAspectRatio = defaultSvg
    }

-- | Define an `<use>` for a named content.
-- Every named content can be reused in the
-- document using this element.
data Use = Use
  { -- | Position where to draw the "used" element.
    -- Correspond to the `x` and `y` attributes.
    _useBase   :: Point
    -- | Referenced name, correspond to `xlink:href`
    -- attribute.
  , _useName   :: String
    -- | Define the width of the region where
    -- to place the element. Map to the `width`
    -- attribute.
  , _useWidth  :: Maybe Number
    -- | Define the height of the region where
    -- to place the element. Map to the `height`
    -- attribute.
  , _useHeight :: Maybe Number
    -- | Use draw attributes.
  , _useDrawAttributes :: DrawAttributes
  }
  deriving (Eq, Show)

-- makeClassy ''Use
-- | Lenses for the Use type.
class HasUse c_anR3 where
  use :: Lens' c_anR3 Use
  useBase :: Lens' c_anR3 Point
  {-# INLINE useBase #-}
  useDrawAttributes :: Lens' c_anR3 DrawAttributes
  {-# INLINE useDrawAttributes #-}
  useHeight :: Lens' c_anR3 (Maybe Number)
  {-# INLINE useHeight #-}
  useName :: Lens' c_anR3 String
  {-# INLINE useName #-}
  useWidth :: Lens' c_anR3 (Maybe Number)
  {-# INLINE useWidth #-}
  useBase = ((.) use) useBase
  useDrawAttributes = ((.) use) useDrawAttributes
  useHeight = ((.) use) useHeight
  useName = ((.) use) useName
  useWidth = ((.) use) useWidth
instance HasUse Use where
  {-# INLINE useBase #-}
  {-# INLINE useDrawAttributes #-}
  {-# INLINE useHeight #-}
  {-# INLINE useName #-}
  {-# INLINE useWidth #-}
  use = id
  useBase f attr =
    fmap (\y -> attr { _useBase = y }) (f $ _useBase attr)
  useDrawAttributes f attr =
    fmap (\y -> attr { _useDrawAttributes = y }) (f $ _useDrawAttributes attr)
  useHeight f attr =
    fmap (\y -> attr { _useHeight = y }) (f $ _useHeight attr)
  useName f attr =
    fmap (\y -> attr { _useName = y }) (f $ _useName attr)
  useWidth f attr =
    fmap (\y -> attr { _useWidth = y }) (f $ _useWidth attr)

instance WithDrawAttributes Use where
  drawAttr = useDrawAttributes

instance WithDefaultSvg Use where
  defaultSvg = Use
    { _useBase   = (Num 0, Num 0)
    , _useName   = ""
    , _useWidth  = Nothing
    , _useHeight = Nothing
    , _useDrawAttributes = mempty
    }

-- | Define position information associated to
-- `<text>` or `<tspan>` svg tag.
data TextInfo = TextInfo
  { _textInfoX      :: ![Number] -- ^ `x` attribute.
  , _textInfoY      :: ![Number] -- ^ `y` attribute.
  , _textInfoDX     :: ![Number] -- ^ `dx` attribute.
  , _textInfoDY     :: ![Number] -- ^ `dy` attribute.
  , _textInfoRotate :: ![Double] -- ^ `rotate` attribute.
  , _textInfoLength :: !(Maybe Number) -- ^ `textLength` attribute.
  }
  deriving (Eq, Show)

instance Semigroup TextInfo where
  (<>) (TextInfo x1 y1 dx1 dy1 r1 l1)
       (TextInfo x2 y2 dx2 dy2 r2 l2) =
    TextInfo (x1 <> x2)   (y1 <> y2)
                (dx1 <> dx2) (dy1 <> dy2)
                (r1 <> r2)
                (getLast $ Last l1 <> Last l2)

instance Monoid TextInfo where
  mempty = TextInfo [] [] [] [] [] Nothing
  mappend = (<>)

-- makeClassy ''TextInfo
-- | Lenses for the TextInfo type.
class HasTextInfo c_ao0m where
  textInfo :: Lens' c_ao0m TextInfo
  textInfoDX :: Lens' c_ao0m [Number]
  {-# INLINE textInfoDX #-}
  textInfoDY :: Lens' c_ao0m [Number]
  {-# INLINE textInfoDY #-}
  textInfoLength :: Lens' c_ao0m (Maybe Number)
  {-# INLINE textInfoLength #-}
  textInfoRotate :: Lens' c_ao0m [Double]
  {-# INLINE textInfoRotate #-}
  textInfoX :: Lens' c_ao0m [Number]
  {-# INLINE textInfoX #-}
  textInfoY :: Lens' c_ao0m [Number]
  {-# INLINE textInfoY #-}
  textInfoDX = ((.) textInfo) textInfoDX
  textInfoDY = ((.) textInfo) textInfoDY
  textInfoLength = ((.) textInfo) textInfoLength
  textInfoRotate = ((.) textInfo) textInfoRotate
  textInfoX = ((.) textInfo) textInfoX
  textInfoY = ((.) textInfo) textInfoY
instance HasTextInfo TextInfo where
  {-# INLINE textInfoDX #-}
  {-# INLINE textInfoDY #-}
  {-# INLINE textInfoLength #-}
  {-# INLINE textInfoRotate #-}
  {-# INLINE textInfoX #-}
  {-# INLINE textInfoY #-}
  textInfo = id
  textInfoDX f attr =
    fmap (\y -> attr { _textInfoDX = y }) (f $ _textInfoDX attr)
  textInfoDY f attr =
    fmap (\y -> attr { _textInfoDY = y }) (f $ _textInfoDY attr)
  textInfoLength f attr =
    fmap (\y -> attr { _textInfoLength = y }) (f $ _textInfoLength attr)
  textInfoRotate f attr =
    fmap (\y -> attr { _textInfoRotate = y }) (f $ _textInfoRotate attr)
  textInfoX f attr =
    fmap (\y -> attr { _textInfoX = y }) (f $ _textInfoX attr)
  textInfoY f attr =
    fmap (\y -> attr { _textInfoY = y }) (f $ _textInfoY attr)

instance WithDefaultSvg TextInfo where
  defaultSvg = mempty

-- | Define the content of a `<tspan>` tag.
data TextSpanContent
    = SpanText    !T.Text -- ^ Raw text
    | SpanTextRef !String -- ^ Equivalent to a `<tref>`
    | SpanSub     !TextSpan -- ^ Define a `<tspan>`
    deriving (Eq, Show)

-- | Define a `<tspan>` tag.
data TextSpan = TextSpan
  { -- | Placing information for the text.
    _spanInfo           :: !TextInfo
    -- | Drawing attributes for the text span.
  , _spanDrawAttributes :: !DrawAttributes
    -- | Content of the span.
  , _spanContent        :: ![TextSpanContent]
  }
  deriving (Eq, Show)

-- makeClassy ''TextSpan
-- | Lenses for the TextSpan type.
class HasTextSpan c_aobD where
  textSpan :: Lens' c_aobD TextSpan
  spanContent :: Lens' c_aobD [TextSpanContent]
  {-# INLINE spanContent #-}
  spanDrawAttributes :: Lens' c_aobD DrawAttributes
  {-# INLINE spanDrawAttributes #-}
  spanInfo :: Lens' c_aobD TextInfo
  {-# INLINE spanInfo #-}
  spanContent = ((.) textSpan) spanContent
  spanDrawAttributes = ((.) textSpan) spanDrawAttributes
  spanInfo = ((.) textSpan) spanInfo
instance HasTextSpan TextSpan where
  {-# INLINE spanContent #-}
  {-# INLINE spanDrawAttributes #-}
  {-# INLINE spanInfo #-}
  textSpan = id
  spanContent f attr =
    fmap (\y -> attr { _spanContent = y }) (f $ _spanContent attr)
  spanDrawAttributes f attr =
    fmap (\y -> attr { _spanDrawAttributes = y }) (f $ _spanDrawAttributes attr)
  spanInfo f attr =
    fmap (\y -> attr { _spanInfo = y }) (f $ _spanInfo attr)

instance WithDefaultSvg TextSpan where
  defaultSvg = TextSpan
    { _spanInfo = defaultSvg
    , _spanDrawAttributes = mempty
    , _spanContent        = mempty
    }

-- | Describe the content of the `method` attribute on
-- text path.
data TextPathMethod
  = TextPathAlign   -- ^ Map to the `align` value.
  | TextPathStretch -- ^ Map to the `stretch` value.
  deriving (Eq, Show)

-- | Describe the content of the `spacing` text path
-- attribute.
data TextPathSpacing
  = TextPathSpacingExact -- ^ Map to the `exact` value.
  | TextPathSpacingAuto  -- ^ Map to the `auto` value.
  deriving (Eq, Show)

-- | Describe the `<textpath>` SVG tag.
data TextPath = TextPath
  { -- | Define the beginning offset on the path,
    -- the `startOffset` attribute.
    _textPathStartOffset :: !Number
    -- | Define the `xlink:href` attribute.
  , _textPathName        :: !String
    -- | Correspond to the `method` attribute.
  , _textPathMethod      :: !TextPathMethod
    -- | Correspond to the `spacing` attribute.
  , _textPathSpacing     :: !TextPathSpacing
    -- | Real content of the path.
  , _textPathData        :: ![PathCommand]
  }
  deriving (Eq, Show)

-- makeClassy ''TextPath
-- | Lenses for the TextPath type.
class HasTextPath c_aojU where
  textPath :: Lens' c_aojU TextPath
  textPathData :: Lens' c_aojU [PathCommand]
  {-# INLINE textPathData #-}
  textPathMethod :: Lens' c_aojU TextPathMethod
  {-# INLINE textPathMethod #-}
  textPathName :: Lens' c_aojU String
  {-# INLINE textPathName #-}
  textPathSpacing :: Lens' c_aojU TextPathSpacing
  {-# INLINE textPathSpacing #-}
  textPathStartOffset :: Lens' c_aojU Number
  {-# INLINE textPathStartOffset #-}
  textPathData = ((.) textPath) textPathData
  textPathMethod = ((.) textPath) textPathMethod
  textPathName = ((.) textPath) textPathName
  textPathSpacing = ((.) textPath) textPathSpacing
  textPathStartOffset = ((.) textPath) textPathStartOffset
instance HasTextPath TextPath where
  {-# INLINE textPathData #-}
  {-# INLINE textPathMethod #-}
  {-# INLINE textPathName #-}
  {-# INLINE textPathSpacing #-}
  {-# INLINE textPathStartOffset #-}
  textPath = id
  textPathData f attr =
    fmap (\y -> attr { _textPathData = y }) (f $ _textPathData attr)
  textPathMethod f attr =
    fmap (\y -> attr { _textPathMethod = y }) (f $ _textPathMethod attr)
  textPathName f attr =
    fmap (\y -> attr { _textPathName = y }) (f $ _textPathName attr)
  textPathSpacing f attr =
    fmap (\y -> attr { _textPathSpacing = y }) (f $ _textPathSpacing attr)
  textPathStartOffset f attr =
    fmap (\y -> attr { _textPathStartOffset = y }) (f $ _textPathStartOffset attr)

instance WithDefaultSvg TextPath where
  defaultSvg = TextPath
    { _textPathStartOffset = Num 0
    , _textPathName        = mempty
    , _textPathMethod      = TextPathAlign
    , _textPathSpacing     = TextPathSpacingExact
    , _textPathData        = []
    }

-- | Define the possible values of the `lengthAdjust`
-- attribute.
data TextAdjust
  = TextAdjustSpacing -- ^ Value `spacing`
  | TextAdjustSpacingAndGlyphs -- ^ Value `spacingAndGlyphs`
  deriving (Eq, Show)

-- | Define the global `<tag>` SVG tag.
data Text = Text
  { -- | Define the `lengthAdjust` attribute.
    _textAdjust   :: !TextAdjust
    -- | Root of the text content.
  , _textRoot     :: !TextSpan
  }
  deriving (Eq, Show)

-- makeClassy ''Text
-- | Lenses for the Text type.
class HasText c_aorD where
  text :: Lens' c_aorD Text
  textAdjust :: Lens' c_aorD TextAdjust
  {-# INLINE textAdjust #-}
  textRoot :: Lens' c_aorD TextSpan
  {-# INLINE textRoot #-}
  textAdjust = ((.) text) textAdjust
  textRoot = ((.) text) textRoot
instance HasText Text where
  {-# INLINE textAdjust #-}
  {-# INLINE textRoot #-}
  text = id
  textAdjust f attr =
    fmap (\y -> attr { _textAdjust = y }) (f $ _textAdjust attr)
  textRoot f attr =
    fmap (\y -> attr { _textRoot = y }) (f $ _textRoot attr)

-- | Little helper to create a SVG text at a given
-- baseline position.
textAt :: Point -> T.Text -> Text
textAt (x, y) txt = Text TextAdjustSpacing tspan where
  tspan = defaultSvg
        { _spanContent = [SpanText txt]
        , _spanInfo = defaultSvg
                    { _textInfoX = [x]
                    , _textInfoY = [y]
                    }
        }

instance WithDrawAttributes Text where
  drawAttr = textRoot . spanDrawAttributes

instance WithDefaultSvg Text where
  defaultSvg = Text
    { _textRoot = defaultSvg
    , _textAdjust = TextAdjustSpacing
    }

-- | Main type for the scene description, reorient to
-- specific type describing each tag.
data Tree
    = None
    | UseTree { useInformation :: !Use
              , useSubTree     :: !(Maybe Tree) }
    | GroupTree     !(Group Tree)
    | SymbolTree    !(Symbol Tree)
    | PathTree      !Path
    | CircleTree    !Circle
    | PolyLineTree  !PolyLine
    | PolygonTree   !Polygon
    | EllipseTree   !Ellipse
    | LineTree      !Line
    | RectangleTree !Rectangle
    | TextTree      !(Maybe TextPath) !Text
    | ImageTree     !Image
    | MeshGradientTree !MeshGradient
    deriving (Eq, Show)

-- | Define the orientation, associated to the
-- `orient` attribute on the Marker
data MarkerOrientation
  = OrientationAuto        -- ^ Auto value
  | OrientationAngle Coord -- ^ Specific angle.
  deriving (Eq, Show)

-- | Define the content of the `markerUnits` attribute
-- on the Marker.
data MarkerUnit
  = MarkerUnitStrokeWidth    -- ^ Value `strokeWidth`
  | MarkerUnitUserSpaceOnUse -- ^ Value `userSpaceOnUse`
  deriving (Eq, Show)

-- | Define the content of the `markerUnits` attribute
-- on the Marker.
data Overflow
  = OverflowVisible    -- ^ Value `visible`
  | OverflowHidden     -- ^ Value `hidden`
  deriving (Eq, Show)

-- | Define the `<marker>` tag.
data Marker = Marker
  { -- | Draw attributes of the marker.
    _markerDrawAttributes :: DrawAttributes
    -- | Define the reference point of the marker.
    -- correspond to the `refX` and `refY` attributes.
  , _markerRefPoint    :: !(Number, Number)
    -- | Define the width of the marker. Correspond to
    -- the `markerWidth` attribute.
  , _markerWidth       :: !(Maybe Number)
    -- | Define the height of the marker. Correspond to
    -- the `markerHeight` attribute.
  , _markerHeight      :: !(Maybe Number)
    -- | Correspond to the `orient` attribute.
  , _markerOrient      :: !(Maybe MarkerOrientation)
    -- | Map the `markerUnits` attribute.
  , _markerUnits       :: !(Maybe MarkerUnit)
    -- | Optional viewbox
  , _markerViewBox     :: !(Maybe (Double, Double, Double, Double))
    -- | Elements defining the marker.
  , _markerOverflow    :: !(Maybe Overflow)
    -- | preserveAspectRatio attribute
  , _markerAspectRatio :: !PreserveAspectRatio
    -- | Elements defining the marker.
  , _markerElements :: [Tree]
  }
  deriving (Eq, Show)

-- makeClassy ''Marker
-- | Lenses for the Marker type.
class HasMarker c_aoKc where
  marker :: Lens' c_aoKc Marker
  markerAspectRatio :: Lens' c_aoKc PreserveAspectRatio
  {-# INLINE markerAspectRatio #-}
  markerDrawAttributes :: Lens' c_aoKc DrawAttributes
  {-# INLINE markerDrawAttributes #-}
  markerElements :: Lens' c_aoKc [Tree]
  {-# INLINE markerElements #-}
  markerHeight :: Lens' c_aoKc (Maybe Number)
  {-# INLINE markerHeight #-}
  markerOrient :: Lens' c_aoKc (Maybe MarkerOrientation)
  {-# INLINE markerOrient #-}
  markerOverflow :: Lens' c_aoKc (Maybe Overflow)
  {-# INLINE markerOverflow #-}
  markerRefPoint :: Lens' c_aoKc (Number, Number)
  {-# INLINE markerRefPoint #-}
  markerUnits :: Lens' c_aoKc (Maybe MarkerUnit)
  {-# INLINE markerUnits #-}
  markerViewBox ::
    Lens' c_aoKc (Maybe (Double, Double, Double, Double))
  {-# INLINE markerViewBox #-}
  markerWidth :: Lens' c_aoKc (Maybe Number)
  {-# INLINE markerWidth #-}
  markerAspectRatio = ((.) marker) markerAspectRatio
  markerDrawAttributes = ((.) marker) markerDrawAttributes
  markerElements = ((.) marker) markerElements
  markerHeight = ((.) marker) markerHeight
  markerOrient = ((.) marker) markerOrient
  markerOverflow = ((.) marker) markerOverflow
  markerRefPoint = ((.) marker) markerRefPoint
  markerUnits = ((.) marker) markerUnits
  markerViewBox = ((.) marker) markerViewBox
  markerWidth = ((.) marker) markerWidth
instance HasMarker Marker where
  {-# INLINE markerAspectRatio #-}
  {-# INLINE markerDrawAttributes #-}
  {-# INLINE markerElements #-}
  {-# INLINE markerHeight #-}
  {-# INLINE markerOrient #-}
  {-# INLINE markerOverflow #-}
  {-# INLINE markerRefPoint #-}
  {-# INLINE markerUnits #-}
  {-# INLINE markerViewBox #-}
  {-# INLINE markerWidth #-}
  marker = id
  markerAspectRatio f attr =
    fmap (\y -> attr { _markerAspectRatio = y }) (f $ _markerAspectRatio attr)
  markerDrawAttributes f attr =
    fmap (\y -> attr { _markerDrawAttributes = y }) (f $ _markerDrawAttributes attr)
  markerElements f attr =
    fmap (\y -> attr { _markerElements = y }) (f $ _markerElements attr)
  markerHeight f attr =
    fmap (\y -> attr { _markerHeight = y }) (f $ _markerHeight attr)
  markerOrient f attr =
    fmap (\y -> attr { _markerOrient = y }) (f $ _markerOrient attr)
  markerOverflow f attr =
    fmap (\y -> attr { _markerOverflow = y }) (f $ _markerOverflow attr)
  markerRefPoint f attr =
    fmap (\y -> attr { _markerRefPoint = y }) (f $ _markerRefPoint attr)
  markerUnits f attr =
    fmap (\y -> attr { _markerUnits = y }) (f $ _markerUnits attr)
  markerViewBox f attr =
    fmap (\y -> attr { _markerViewBox = y }) (f $ _markerViewBox attr)
  markerWidth f attr =
    fmap (\y -> attr { _markerWidth = y }) (f $ _markerWidth attr)

instance WithDrawAttributes Marker where
    drawAttr = markerDrawAttributes

instance WithDefaultSvg Marker where
  defaultSvg = Marker
    { _markerDrawAttributes = mempty
    , _markerRefPoint = (Num 0, Num 0)
    , _markerWidth = Just (Num 3)
    , _markerHeight = Just (Num 3)
    , _markerOrient = Nothing -- MarkerOrientation
    , _markerUnits = Nothing -- MarkerUnitStrokeWidth
    , _markerViewBox = Nothing
    , _markerOverflow = Nothing
    , _markerElements = mempty
    , _markerAspectRatio = defaultSvg
    }

-- | Insert element in the first sublist in the list of list.
appNode :: [[a]] -> a -> [[a]]
appNode [] e = [[e]]
appNode (curr:above) e = (e:curr) : above

-- | Map a tree while propagating context information.
-- The function passed in parameter receive a list
-- representing the the path used to go arrive to the
-- current node.
zipTree :: ([[Tree]] -> Tree) -> Tree -> Tree
zipTree f = dig [] where
  dig prev e@None = f $ appNode prev e
  dig prev e@(UseTree _ Nothing) = f $ appNode prev e
  dig prev e@(UseTree nfo (Just u)) =
      f . appNode prev . UseTree nfo . Just $ dig ([] : appNode prev e) u
  dig prev e@(GroupTree g) =
      f . appNode prev . GroupTree $ zipGroup (appNode prev e) g
  dig prev e@(SymbolTree g) =
      f . appNode prev . SymbolTree . Symbol .
            zipGroup (appNode prev e) $ _groupOfSymbol g
  dig prev e@(PathTree _) = f $ appNode prev e
  dig prev e@(CircleTree _) = f $ appNode prev e
  dig prev e@(PolyLineTree _) = f $ appNode prev e
  dig prev e@(PolygonTree _) = f $ appNode prev e
  dig prev e@(EllipseTree _) = f $ appNode prev e
  dig prev e@(LineTree _) = f $ appNode prev e
  dig prev e@(RectangleTree _) = f $ appNode prev e
  dig prev e@(TextTree _ _) = f $ appNode prev e
  dig prev e@(ImageTree _) = f $ appNode prev e
  dig prev e@(MeshGradientTree _) = f $ appNode prev e

  zipGroup prev g = g { _groupChildren = updatedChildren }
    where
      groupChild = _groupChildren g
      updatedChildren =
        [dig (c:prev) child
            | (child, c) <- zip groupChild $ inits groupChild]

-- | Fold all nodes of a SVG tree.
foldTree :: (a -> Tree -> a) -> a -> Tree -> a
foldTree f = go where
  go acc e = case e of
    None            -> f acc e
    UseTree _ _     -> f acc e
    PathTree _      -> f acc e
    CircleTree _    -> f acc e
    PolyLineTree _  -> f acc e
    PolygonTree _   -> f acc e
    EllipseTree _   -> f acc e
    LineTree _      -> f acc e
    RectangleTree _ -> f acc e
    TextTree    _ _ -> f acc e
    ImageTree _     -> f acc e
    MeshGradientTree _ -> f acc e
    GroupTree g     ->
      let subAcc = F.foldl' go acc $ _groupChildren g in
      f subAcc e
    SymbolTree s    ->
      let subAcc =
            F.foldl' go acc . _groupChildren $ _groupOfSymbol s in
      f subAcc e

-- | Helper function mapping every tree element.
mapTree :: (Tree -> Tree) -> Tree -> Tree
mapTree f = go where
  go e@None = f e
  go e@(UseTree _ _) = f e
  go (GroupTree g) = f . GroupTree $ mapGroup g
  go (SymbolTree g) =
      f . SymbolTree . Symbol . mapGroup $ _groupOfSymbol g
  go e@(PathTree _) = f e
  go e@(CircleTree _) = f e
  go e@(PolyLineTree _) = f e
  go e@(PolygonTree _) = f e
  go e@(EllipseTree _) = f e
  go e@(LineTree _) = f e
  go e@(RectangleTree _) = f e
  go e@(TextTree _ _) = f e
  go e@(ImageTree _) = f e
  go e@(MeshGradientTree _) = f e

  mapGroup g =
      g { _groupChildren = map go $ _groupChildren g }

-- | For every element of a svg tree, associate
-- it's SVG tag name.
nameOfTree :: Tree -> T.Text
nameOfTree v =
  case v of
   None     -> ""
   UseTree _ _     -> "use"
   GroupTree _     -> "g"
   SymbolTree _    -> "symbol"
   PathTree _      -> "path"
   CircleTree _    -> "circle"
   PolyLineTree _  -> "polyline"
   PolygonTree _   -> "polygon"
   EllipseTree _   -> "ellipse"
   LineTree _      -> "line"
   RectangleTree _ -> "rectangle"
   TextTree    _ _ -> "text"
   ImageTree _     -> "image"
   MeshGradientTree _ -> "meshgradient"

drawAttrOfTree :: Tree -> DrawAttributes
drawAttrOfTree v = case v of
  None -> mempty
  UseTree e _ -> e ^. drawAttr
  GroupTree e -> e ^. drawAttr
  SymbolTree e -> e ^. drawAttr
  PathTree e -> e ^. drawAttr
  CircleTree e -> e ^. drawAttr
  PolyLineTree e -> e ^. drawAttr
  PolygonTree e -> e ^. drawAttr
  EllipseTree e -> e ^. drawAttr
  LineTree e -> e ^. drawAttr
  RectangleTree e -> e ^. drawAttr
  TextTree _ e -> e ^. drawAttr
  ImageTree e -> e ^. drawAttr
  MeshGradientTree e -> e ^. drawAttr

setDrawAttrOfTree :: Tree -> DrawAttributes -> Tree
setDrawAttrOfTree v attr = case v of
  None -> None
  UseTree e m -> UseTree (e & drawAttr .~ attr) m
  GroupTree e -> GroupTree $ e & drawAttr .~ attr
  SymbolTree e -> SymbolTree $ e & drawAttr .~ attr
  PathTree e -> PathTree $ e & drawAttr .~ attr
  CircleTree e -> CircleTree $ e & drawAttr .~ attr
  PolyLineTree e -> PolyLineTree $ e & drawAttr .~ attr
  PolygonTree e -> PolygonTree $ e & drawAttr .~ attr
  EllipseTree e -> EllipseTree $ e & drawAttr .~ attr
  LineTree e -> LineTree $ e & drawAttr .~ attr
  RectangleTree e -> RectangleTree $ e & drawAttr .~ attr
  TextTree a e -> TextTree a $ e & drawAttr .~ attr
  ImageTree e -> ImageTree $ e & drawAttr .~ attr
  MeshGradientTree e -> MeshGradientTree $ e & drawAttr .~ attr

instance WithDrawAttributes Tree where
    drawAttr = lens drawAttrOfTree setDrawAttrOfTree

instance WithDefaultSvg Tree where
    defaultSvg = None

-- | Define the possible values for the `spreadMethod`
-- values used for the gradient definitions.
data Spread
    = SpreadRepeat  -- ^ `reapeat` value
    | SpreadPad     -- ^ `pad` value
    | SpreadReflect -- ^ `reflect value`
    deriving (Eq, Show)

-- | Define a `<linearGradient>` tag.
data LinearGradient = LinearGradient
    { -- | Define coordinate system of the gradient,
      -- associated to the `gradientUnits` attribute.
      _linearGradientUnits  :: CoordinateUnits
      -- | Point defining the beginning of the line gradient.
      -- Associated to the `x1` and `y1` attribute.
    , _linearGradientStart  :: Point
      -- | Point defining the end of the line gradient.
      -- Associated to the `x2` and `y2` attribute.
    , _linearGradientStop   :: Point
      -- | Define how to handle the values outside
      -- the gradient start and stop. Associated to the
      -- `spreadMethod` attribute.
    , _linearGradientSpread :: Spread
      -- | Define the transformation to apply to the
      -- gradient points. Associated to the `gradientTransform`
      -- attribute.
    , _linearGradientTransform :: [Transformation]
      -- | List of color stops of the linear gradient.
    , _linearGradientStops  :: [GradientStop]
    }
    deriving (Eq, Show)

-- makeClassy ''LinearGradient
-- | Lenses for the LinearGradient type.
class HasLinearGradient c_apmJ where
  linearGradient :: Lens' c_apmJ LinearGradient
  linearGradientSpread :: Lens' c_apmJ Spread
  {-# INLINE linearGradientSpread #-}
  linearGradientStart :: Lens' c_apmJ Point
  {-# INLINE linearGradientStart #-}
  linearGradientStop :: Lens' c_apmJ Point
  {-# INLINE linearGradientStop #-}
  linearGradientStops :: Lens' c_apmJ [GradientStop]
  {-# INLINE linearGradientStops #-}
  linearGradientTransform :: Lens' c_apmJ [Transformation]
  {-# INLINE linearGradientTransform #-}
  linearGradientUnits :: Lens' c_apmJ CoordinateUnits
  {-# INLINE linearGradientUnits #-}
  linearGradientSpread = ((.) linearGradient) linearGradientSpread
  linearGradientStart = ((.) linearGradient) linearGradientStart
  linearGradientStop = ((.) linearGradient) linearGradientStop
  linearGradientStops = ((.) linearGradient) linearGradientStops
  linearGradientTransform
    = ((.) linearGradient) linearGradientTransform
  linearGradientUnits = ((.) linearGradient) linearGradientUnits

instance HasLinearGradient LinearGradient where
  {-# INLINE linearGradientSpread #-}
  {-# INLINE linearGradientStart #-}
  {-# INLINE linearGradientStop #-}
  {-# INLINE linearGradientStops #-}
  {-# INLINE linearGradientTransform #-}
  {-# INLINE linearGradientUnits #-}
  linearGradient = id
  linearGradientSpread f attr =
    fmap (\y -> attr { _linearGradientSpread = y }) (f $ _linearGradientSpread attr)
  linearGradientStart f attr =
    fmap (\y -> attr { _linearGradientStart = y }) (f $ _linearGradientStart attr)
  linearGradientStop f attr =
    fmap (\y -> attr { _linearGradientStop = y }) (f $ _linearGradientStop attr)
  linearGradientStops f attr =
    fmap (\y -> attr { _linearGradientStops = y }) (f $ _linearGradientStops attr)
  linearGradientTransform f attr =
    fmap (\y -> attr { _linearGradientTransform = y }) (f $ _linearGradientTransform attr)
  linearGradientUnits f attr =
    fmap (\y -> attr { _linearGradientUnits = y }) (f $ _linearGradientUnits attr)

instance WithDefaultSvg LinearGradient where
  defaultSvg = LinearGradient
    { _linearGradientUnits     = CoordBoundingBox
    , _linearGradientStart     = (Percent 0, Percent 0)
    , _linearGradientStop      = (Percent 1, Percent 0)
    , _linearGradientSpread    = SpreadPad
    , _linearGradientTransform = []
    , _linearGradientStops     = []
    }

-- | Define a `<radialGradient>` tag.
data RadialGradient = RadialGradient
  { -- | Define coordinate system of the gradient,
    -- associated to the `gradientUnits` attribute.
    _radialGradientUnits   :: CoordinateUnits
    -- | Center of the radial gradient. Associated to
    -- the `cx` and `cy` attributes.
  , _radialGradientCenter  :: Point
    -- | Radius of the radial gradient. Associated to
    -- the `r` attribute.
  , _radialGradientRadius  :: Number
    -- | X coordinate of the focus point of the radial
    -- gradient. Associated to the `fx` attribute.
  , _radialGradientFocusX  :: Maybe Number
    -- | Y coordinate of the focus point of the radial
    -- gradient. Associated to the `fy` attribute.
  , _radialGradientFocusY  :: Maybe Number
    -- | Define how to handle the values outside
    -- the gradient start and stop. Associated to the
    -- `spreadMethod` attribute.
  , _radialGradientSpread  :: Spread
    -- | Define the transformation to apply to the
    -- gradient points. Associated to the `gradientTransform`
    -- attribute.
  , _radialGradientTransform :: [Transformation]
    -- | List of color stops of the radial gradient.
  , _radialGradientStops   :: [GradientStop]
  }
  deriving (Eq, Show)

-- makeClassy ''RadialGradient
-- | Lenses for the RadialGradient type.

class HasRadialGradient c_apwt where
  radialGradient :: Lens' c_apwt RadialGradient
  radialGradientCenter :: Lens' c_apwt Point
  {-# INLINE radialGradientCenter #-}
  radialGradientFocusX :: Lens' c_apwt (Maybe Number)
  {-# INLINE radialGradientFocusX #-}
  radialGradientFocusY :: Lens' c_apwt (Maybe Number)
  {-# INLINE radialGradientFocusY #-}
  radialGradientRadius :: Lens' c_apwt Number
  {-# INLINE radialGradientRadius #-}
  radialGradientSpread :: Lens' c_apwt Spread
  {-# INLINE radialGradientSpread #-}
  radialGradientStops :: Lens' c_apwt [GradientStop]
  {-# INLINE radialGradientStops #-}
  radialGradientTransform :: Lens' c_apwt [Transformation]
  {-# INLINE radialGradientTransform #-}
  radialGradientUnits :: Lens' c_apwt CoordinateUnits
  {-# INLINE radialGradientUnits #-}
  radialGradientCenter = ((.) radialGradient) radialGradientCenter
  radialGradientFocusX = ((.) radialGradient) radialGradientFocusX
  radialGradientFocusY = ((.) radialGradient) radialGradientFocusY
  radialGradientRadius = ((.) radialGradient) radialGradientRadius
  radialGradientSpread = ((.) radialGradient) radialGradientSpread
  radialGradientStops = ((.) radialGradient) radialGradientStops
  radialGradientTransform
    = ((.) radialGradient) radialGradientTransform
  radialGradientUnits = ((.) radialGradient) radialGradientUnits

instance HasRadialGradient RadialGradient where
  {-# INLINE radialGradientCenter #-}
  {-# INLINE radialGradientFocusX #-}
  {-# INLINE radialGradientFocusY #-}
  {-# INLINE radialGradientRadius #-}
  {-# INLINE radialGradientSpread #-}
  {-# INLINE radialGradientStops #-}
  {-# INLINE radialGradientTransform #-}
  {-# INLINE radialGradientUnits #-}
  radialGradient = id
  radialGradientCenter f attr =
    fmap (\y -> attr { _radialGradientCenter = y }) (f $ _radialGradientCenter attr)
  radialGradientFocusX f attr =
    fmap (\y -> attr { _radialGradientFocusX = y }) (f $ _radialGradientFocusX attr)
  radialGradientFocusY f attr =
    fmap (\y -> attr { _radialGradientFocusY = y }) (f $ _radialGradientFocusY attr)
  radialGradientRadius f attr =
    fmap (\y -> attr { _radialGradientRadius = y }) (f $ _radialGradientRadius attr)
  radialGradientSpread f attr =
    fmap (\y -> attr { _radialGradientSpread = y }) (f $ _radialGradientSpread attr)
  radialGradientStops f attr =
    fmap (\y -> attr { _radialGradientStops = y }) (f $ _radialGradientStops attr)
  radialGradientTransform f attr =
    fmap (\y -> attr { _radialGradientTransform = y }) (f $ _radialGradientTransform attr)
  radialGradientUnits f attr =
    fmap (\y -> attr { _radialGradientUnits = y }) (f $ _radialGradientUnits attr)

instance WithDefaultSvg RadialGradient where
  defaultSvg = RadialGradient
    { _radialGradientUnits   = CoordBoundingBox
    , _radialGradientCenter  = (Percent 0.5, Percent 0.5)
    , _radialGradientRadius  = Percent 0.5
    , _radialGradientFocusX  = Nothing
    , _radialGradientFocusY  = Nothing
    , _radialGradientSpread  = SpreadPad
    , _radialGradientTransform = []
    , _radialGradientStops   = []
    }

-- | Define a SVG `<mask>` tag.
data Mask = Mask
  { -- | Drawing attributes of the Mask
    _maskDrawAttributes :: DrawAttributes
    -- | Correspond to the `maskContentUnits` attributes.
  , _maskContentUnits :: CoordinateUnits
    -- | Mapping to the `maskUnits` attribute.
  , _maskUnits        :: CoordinateUnits
    -- | Map to the `x` and `y` attributes.
  , _maskPosition     :: Point
    -- | Map to the `width` attribute
  , _maskWidth        :: Number
    -- | Map to the `height` attribute.
  , _maskHeight       :: Number
    -- | Children of the `<mask>` tag.
  , _maskContent      :: [Tree]
  }
  deriving (Eq, Show)

-- makeClassy ''Mask
-- | Lenses for the Mask type.
class HasMask c_apHI where
  mask :: Lens' c_apHI Mask
  maskContent :: Lens' c_apHI [Tree]
  {-# INLINE maskContent #-}
  maskContentUnits :: Lens' c_apHI CoordinateUnits
  {-# INLINE maskContentUnits #-}
  maskDrawAttributes :: Lens' c_apHI DrawAttributes
  {-# INLINE maskDrawAttributes #-}
  maskHeight :: Lens' c_apHI Number
  {-# INLINE maskHeight #-}
  maskPosition :: Lens' c_apHI Point
  {-# INLINE maskPosition #-}
  maskUnits :: Lens' c_apHI CoordinateUnits
  {-# INLINE maskUnits #-}
  maskWidth :: Lens' c_apHI Number
  {-# INLINE maskWidth #-}
  maskContent = ((.) mask) maskContent
  maskContentUnits = ((.) mask) maskContentUnits
  maskDrawAttributes = ((.) mask) maskDrawAttributes
  maskHeight = ((.) mask) maskHeight
  maskPosition = ((.) mask) maskPosition
  maskUnits = ((.) mask) maskUnits
  maskWidth = ((.) mask) maskWidth

instance HasMask Mask where
  {-# INLINE maskContent #-}
  {-# INLINE maskContentUnits #-}
  {-# INLINE maskDrawAttributes #-}
  {-# INLINE maskHeight #-}
  {-# INLINE maskPosition #-}
  {-# INLINE maskUnits #-}
  {-# INLINE maskWidth #-}
  mask = id
  maskContent f attr =
    fmap (\y -> attr { _maskContent = y }) (f $ _maskContent attr)
  maskContentUnits f attr =
    fmap (\y -> attr { _maskContentUnits = y }) (f $ _maskContentUnits attr)
  maskDrawAttributes f attr =
    fmap (\y -> attr { _maskDrawAttributes = y }) (f $ _maskDrawAttributes attr)
  maskHeight f attr =
    fmap (\y -> attr { _maskHeight = y }) (f $ _maskHeight attr)
  maskPosition f attr =
    fmap (\y -> attr { _maskPosition = y }) (f $ _maskPosition attr)
  maskUnits f attr =
    fmap (\y -> attr { _maskUnits = y }) (f $ _maskUnits attr)
  maskWidth f attr =
    fmap (\y -> attr { _maskWidth = y }) (f $ _maskWidth attr)

instance WithDrawAttributes Mask where
  drawAttr = maskDrawAttributes

instance WithDefaultSvg Mask where
  defaultSvg = Mask
    { _maskDrawAttributes = mempty
    , _maskContentUnits = CoordUserSpace
    , _maskUnits        = CoordBoundingBox
    , _maskPosition     = (Percent (-0.1), Percent (-0.1))
    , _maskWidth        = Percent 1.2
    , _maskHeight       = Percent 1.2
    , _maskContent      = []
    }

-- | Define a `<clipPath>` tag.
data ClipPath = ClipPath
  { _clipPathDrawAttributes :: DrawAttributes
    -- | Maps to the `clipPathUnits` attribute
  , _clipPathUnits          :: CoordinateUnits
    -- | Maps to the content of the tree
  , _clipPathContent        :: [Tree]
  }
  deriving (Eq, Show)

-- makeClassy ''ClipPath
-- | Lenses for the ClipPath type.
class HasClipPath c_apZq where
  clipPath :: Lens' c_apZq ClipPath
  clipPathContent :: Lens' c_apZq [Tree]
  {-# INLINE clipPathContent #-}
  clipPathDrawAttributes :: Lens' c_apZq DrawAttributes
  {-# INLINE clipPathDrawAttributes #-}
  clipPathUnits :: Lens' c_apZq CoordinateUnits
  {-# INLINE clipPathUnits #-}
  clipPathContent = ((.) clipPath) clipPathContent
  clipPathDrawAttributes = ((.) clipPath) clipPathDrawAttributes
  clipPathUnits = ((.) clipPath) clipPathUnits
instance HasClipPath ClipPath where
  {-# INLINE clipPathContent #-}
  {-# INLINE clipPathDrawAttributes #-}
  {-# INLINE clipPathUnits #-}
  clipPath = id
  clipPathContent f attr =
    fmap (\y -> attr { _clipPathContent = y }) (f $ _clipPathContent attr)
  clipPathDrawAttributes f attr =
    fmap (\y -> attr { _clipPathDrawAttributes = y }) (f $ _clipPathDrawAttributes attr)
  clipPathUnits f attr =
    fmap (\y -> attr { _clipPathUnits = y }) (f $ _clipPathUnits attr)

instance WithDrawAttributes ClipPath where
  drawAttr = clipPathDrawAttributes

instance WithDefaultSvg ClipPath where
  defaultSvg = ClipPath
    { _clipPathDrawAttributes = mempty
    , _clipPathUnits = CoordUserSpace
    , _clipPathContent = mempty
    }

-- | Define a `<pattern>` tag.
data Pattern = Pattern
    { -- | Pattern drawing attributes.
      _patternDrawAttributes :: !DrawAttributes
      -- | Possible `viewBox`.
    , _patternViewBox  :: !(Maybe (Double, Double, Double, Double))
      -- | Width of the pattern tile, mapped to the
      -- `width` attribute
    , _patternWidth    :: !Number
      -- | Height of the pattern tile, mapped to the
      -- `height` attribute
    , _patternHeight   :: !Number
      -- | Pattern tile base, mapped to the `x` and
      -- `y` attributes.
    , _patternPos      :: !Point
      -- | Patterns can be chained, so this is a potential
      -- reference to another pattern
    , _patternHref     :: !String
      -- | Elements used in the pattern.
    , _patternElements :: ![Tree]
      -- | Define the cordinate system to use for
      -- the pattern. Mapped to the `patternUnits`
      -- attribute.
    , _patternUnit        :: !CoordinateUnits
      -- | Value of the "preserveAspectRatio" attribute
    , _patternAspectRatio :: !PreserveAspectRatio
      -- | Value of "patternTransform" attribute
    , _patternTransform   :: !(Maybe [Transformation])
    }
    deriving Show

-- makeClassy ''Pattern
-- | Lenses for the Patter type.
class HasPattern c_aq6G where
  pattern :: Lens' c_aq6G Pattern
  patternAspectRatio :: Lens' c_aq6G PreserveAspectRatio
  {-# INLINE patternAspectRatio #-}
  patternDrawAttributes :: Lens' c_aq6G DrawAttributes
  {-# INLINE patternDrawAttributes #-}
  patternElements :: Lens' c_aq6G [Tree]
  {-# INLINE patternElements #-}
  patternHeight :: Lens' c_aq6G Number
  {-# INLINE patternHeight #-}
  patternHref :: Lens' c_aq6G String
  {-# INLINE patternHref #-}
  patternPos :: Lens' c_aq6G Point
  {-# INLINE patternPos #-}
  patternTransform :: Lens' c_aq6G (Maybe [Transformation])
  {-# INLINE patternTransform #-}
  patternUnit :: Lens' c_aq6G CoordinateUnits
  {-# INLINE patternUnit #-}
  patternViewBox ::
    Lens' c_aq6G (Maybe (Double, Double, Double, Double))
  {-# INLINE patternViewBox #-}
  patternWidth :: Lens' c_aq6G Number
  {-# INLINE patternWidth #-}
  patternAspectRatio = ((.) pattern) patternAspectRatio
  patternDrawAttributes = ((.) pattern) patternDrawAttributes
  patternElements = ((.) pattern) patternElements
  patternHeight = ((.) pattern) patternHeight
  patternHref = ((.) pattern) patternHref
  patternPos = ((.) pattern) patternPos
  patternTransform = ((.) pattern) patternTransform
  patternUnit = ((.) pattern) patternUnit
  patternViewBox = ((.) pattern) patternViewBox
  patternWidth = ((.) pattern) patternWidth

instance HasPattern Pattern where
  {-# INLINE patternAspectRatio #-}
  {-# INLINE patternDrawAttributes #-}
  {-# INLINE patternElements #-}
  {-# INLINE patternHeight #-}
  {-# INLINE patternHref #-}
  {-# INLINE patternPos #-}
  {-# INLINE patternTransform #-}
  {-# INLINE patternUnit #-}
  {-# INLINE patternViewBox #-}
  {-# INLINE patternWidth #-}
  pattern = id
  patternAspectRatio f attr =
    fmap (\y -> attr { _patternAspectRatio = y }) (f $ _patternAspectRatio attr)
  patternDrawAttributes f attr =
    fmap (\y -> attr { _patternDrawAttributes = y }) (f $ _patternDrawAttributes attr)
  patternElements f attr =
    fmap (\y -> attr { _patternElements = y }) (f $ _patternElements attr)
  patternHeight f attr =
    fmap (\y -> attr { _patternHeight = y }) (f $ _patternHeight attr)
  patternHref f attr =
    fmap (\y -> attr { _patternHref = y }) (f $ _patternHref attr)
  patternPos f attr =
    fmap (\y -> attr { _patternPos = y }) (f $ _patternPos attr)
  patternTransform f attr =
    fmap (\y -> attr { _patternTransform = y }) (f $ _patternTransform attr)
  patternUnit f attr =
    fmap (\y -> attr { _patternUnit = y }) (f $ _patternUnit attr)
  patternViewBox f attr =
    fmap (\y -> attr { _patternViewBox = y }) (f $ _patternViewBox attr)
  patternWidth f attr =
    fmap (\y -> attr { _patternWidth = y }) (f $ _patternWidth attr)

instance WithDrawAttributes Pattern where
    drawAttr = patternDrawAttributes

instance WithDefaultSvg Pattern where
  defaultSvg = Pattern
    { _patternViewBox  = Nothing
    , _patternWidth    = Num 0
    , _patternHeight   = Num 0
    , _patternPos      = (Num 0, Num 0)
    , _patternElements = []
    , _patternUnit = CoordBoundingBox
    , _patternDrawAttributes = mempty
    , _patternAspectRatio = defaultSvg
    , _patternHref = ""
    , _patternTransform = mempty
    }

-- | Sum types helping keeping track of all the namable
-- elemens in a SVG document.
data Element
    = ElementLinearGradient LinearGradient
    | ElementRadialGradient RadialGradient
    | ElementMeshGradient   MeshGradient
    | ElementGeometry Tree
    | ElementPattern  Pattern
    | ElementMarker Marker
    | ElementMask Mask
    | ElementClipPath ClipPath
    deriving Show

-- | Represent a full svg document with style,
-- geometry and named elements.
data Document = Document
    { _viewBox          :: Maybe (Double, Double, Double, Double)
    , _width            :: Maybe Number
    , _height           :: Maybe Number
    , _elements         :: [Tree]
    , _definitions      :: M.Map String Element
    , _description      :: String
    , _styleRules       :: [CssRule]
    , _documentLocation :: FilePath
    }
    deriving Show

-- makeClassy ''Document
-- | Lenses associated to a SVG document.
class HasDocument c_aqpq where
  document :: Lens' c_aqpq Document
  definitions :: Lens' c_aqpq (M.Map String Element)
  {-# INLINE definitions #-}
  definitions = document . definitions

  description :: Lens' c_aqpq String
  {-# INLINE description #-}
  description = document . description

  documentLocation :: Lens' c_aqpq FilePath
  {-# INLINE documentLocation #-}
  documentLocation = document . documentLocation

  elements :: Lens' c_aqpq [Tree]
  {-# INLINE elements #-}
  elements = document . elements

  height :: Lens' c_aqpq (Maybe Number)
  {-# INLINE height #-}
  height = document . height

  styleRules :: Lens' c_aqpq [CssRule]
  {-# INLINE styleRules #-}
  styleRules = document . styleRules

  viewBox :: Lens' c_aqpq (Maybe (Double, Double, Double, Double))
  {-# INLINE viewBox #-}
  viewBox = document . viewBox

  width :: Lens' c_aqpq (Maybe Number)
  {-# INLINE width #-}
  width = document . width

instance HasDocument Document where
  document = id
  {-# INLINE definitions #-}
  definitions f attr =
    fmap (\y -> attr { _definitions = y }) (f $ _definitions attr)

  {-# INLINE description #-}
  description f attr =
    fmap (\y -> attr { _description = y }) (f $ _description attr)

  {-# INLINE documentLocation #-}
  documentLocation f attr =
    fmap (\y -> attr { _documentLocation = y }) (f $ _documentLocation attr)

  {-# INLINE elements #-}
  elements f attr =
    fmap (\y -> attr { _elements = y }) (f $ _elements attr)

  {-# INLINE height #-}
  height f attr =
    fmap (\y -> attr { _height = y }) (f $ _height attr)

  {-# INLINE styleRules #-}
  styleRules f attr =
    fmap (\y -> attr { _styleRules = y }) (f $ _styleRules attr)

  {-# INLINE viewBox #-}
  viewBox f attr =
    fmap (\y -> attr { _viewBox = y }) (f $ _viewBox attr)

  {-# INLINE width #-}
  width f attr =
    fmap (\y -> attr { _width = y }) (f $ _width attr)

-- | Calculate the document size in function of the
-- different available attributes in the document.
documentSize :: Dpi -> Document -> (Int, Int)
documentSize _ Document { _viewBox = Just (x1, y1, x2, y2)
                        , _width = Just (Percent pw)
                        , _height = Just (Percent ph)
                        } =
    (floor $ dx * pw, floor $ dy * ph)
      where
        dx = abs $ x2 - x1
        dy = abs $ y2 - y1
documentSize _ Document { _width = Just (Num w)
                        , _height = Just (Num h) } = (floor w, floor h)
documentSize dpi doc@(Document { _width = Just w
                               , _height = Just h }) =
  documentSize dpi $ doc
    { _width = Just $ toUserUnit dpi w
    , _height = Just $ toUserUnit dpi h }
documentSize _ Document { _viewBox = Just (x1, y1, x2, y2) } =
    (floor . abs $ x2 - x1, floor . abs $ y2 - y1)
documentSize _ _ = (1, 1)

mayMerge :: Monoid a => Maybe a -> Maybe a -> Maybe a
mayMerge (Just a) (Just b) = Just $ mappend a b
mayMerge _ b@(Just _) = b
mayMerge a Nothing = a

instance Semigroup DrawAttributes where
  (<>) a b = DrawAttributes
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
        , _attrClass = _attrClass b
        , _attrId = _attrId b
        , _groupOpacity = _groupOpacity b
        , _strokeOffset = (mappend `on` _strokeOffset) a b
        , _strokeDashArray = (mappend `on` _strokeDashArray) a b
        , _fontFamily = (mappend `on` _fontFamily) a b
        , _fontStyle = (mappend `on` _fontStyle) a b
        , _textAnchor = (mappend `on` _textAnchor) a b
        , _maskRef = (mappend `on` _maskRef) a b
        , _clipPathRef = (mappend `on` _clipPathRef) a b
        , _clipRule = (mappend `on` _clipRule) a b
        , _markerStart = (mappend `on` _markerStart) a b
        , _markerMid = (mappend `on` _markerMid) a b
        , _markerEnd = (mappend `on` _markerEnd) a b
        }
      where
        opacityMappend Nothing Nothing = Nothing
        opacityMappend (Just v) Nothing = Just v
        opacityMappend Nothing (Just v) = Just v
        opacityMappend (Just v) (Just v2) = Just $ v * v2

instance Monoid DrawAttributes where
    mappend = (<>)
    mempty = DrawAttributes
        { _strokeWidth      = Last Nothing
        , _strokeColor      = Last Nothing
        , _strokeOpacity    = Nothing
        , _strokeLineCap    = Last Nothing
        , _strokeLineJoin   = Last Nothing
        , _strokeMiterLimit = Last Nothing
        , _fillColor        = Last Nothing
        , _groupOpacity     = Nothing
        , _fillOpacity      = Nothing
        , _fontSize         = Last Nothing
        , _fontFamily       = Last Nothing
        , _fontStyle        = Last Nothing
        , _transform        = Nothing
        , _fillRule         = Last Nothing
        , _attrClass        = mempty
        , _attrId           = Nothing
        , _strokeOffset     = Last Nothing
        , _strokeDashArray  = Last Nothing
        , _textAnchor       = Last Nothing
        , _maskRef          = Last Nothing
        , _clipPathRef      = Last Nothing
        , _clipRule         = Last Nothing

        , _markerStart      = Last Nothing
        , _markerMid        = Last Nothing
        , _markerEnd        = Last Nothing
        }

instance WithDefaultSvg DrawAttributes where
  defaultSvg = mempty

instance CssMatcheable Tree where
  cssAttribOf _ _ = Nothing
  cssClassOf = view (drawAttr . attrClass)
  cssIdOf = fmap T.pack . view (drawAttr . attrId)
  cssNameOf = nameOfTree

--------------------------------------------------------------------------
--- Dumped
--------------------------------------------------------------------------
-- makeClassy ''PreserveAspectRatio
--
-- | Lenses for the PreserveAspectRatio type
class HasPreserveAspectRatio a where
  preserveAspectRatio :: Lens' a PreserveAspectRatio
  aspectRatioAlign :: Lens' a Alignment
  {-# INLINE aspectRatioAlign #-}
  aspectRatioAlign = preserveAspectRatio . aspectRatioAlign

  aspectRatioDefer :: Lens' a Bool
  {-# INLINE aspectRatioDefer #-}
  aspectRatioDefer = preserveAspectRatio . aspectRatioDefer

  aspectRatioMeetSlice :: Lens' a (Maybe MeetSlice)
  {-# INLINE aspectRatioMeetSlice #-}
  aspectRatioMeetSlice = preserveAspectRatio . aspectRatioMeetSlice

instance HasPreserveAspectRatio PreserveAspectRatio where
  preserveAspectRatio = id
  {-# INLINE aspectRatioAlign #-}
  aspectRatioAlign f attr =
    fmap (\y -> attr { _aspectRatioAlign = y }) (f $ _aspectRatioAlign attr)

  {-# INLINE aspectRatioDefer #-}
  aspectRatioDefer f attr =
    fmap (\y -> attr { _aspectRatioDefer = y }) (f $ _aspectRatioDefer attr)

  {-# INLINE aspectRatioMeetSlice #-}
  aspectRatioMeetSlice f attr =
    fmap (\y -> attr { _aspectRatioMeetSlice = y }) (f $ _aspectRatioMeetSlice attr)

-- makeClassy ''DrawAttributes
-- | Lenses for the DrawAttributes type.
class HasDrawAttributes a where
  drawAttributes :: Lens' a DrawAttributes
  attrClass :: Lens' a [T.Text]
  {-# INLINE attrClass #-}
  attrClass = drawAttributes . attrClass

  attrId :: Lens' a (Maybe String)
  {-# INLINE attrId #-}
  attrId = drawAttributes . attrId

  clipPathRef :: Lens' a (Last ElementRef)
  {-# INLINE clipPathRef #-}
  clipPathRef = drawAttributes . clipPathRef

  clipRule :: Lens' a (Last FillRule)
  {-# INLINE clipRule #-}
  clipRule = drawAttributes . clipRule

  fillColor :: Lens' a (Last Texture)
  {-# INLINE fillColor #-}
  fillColor = drawAttributes . fillColor

  fillOpacity :: Lens' a (Maybe Float)
  {-# INLINE fillOpacity #-}
  fillOpacity = drawAttributes . fillOpacity

  fillRule :: Lens' a (Last FillRule)
  {-# INLINE fillRule #-}
  fillRule = drawAttributes . fillRule

  fontFamily :: Lens' a (Last [String])
  {-# INLINE fontFamily #-}
  fontFamily = drawAttributes . fontFamily

  fontSize :: Lens' a (Last Number)
  {-# INLINE fontSize #-}
  fontSize = drawAttributes . fontSize

  fontStyle :: Lens' a (Last FontStyle)
  {-# INLINE fontStyle #-}
  fontStyle = drawAttributes . fontStyle

  groupOpacity :: Lens' a (Maybe Float)
  {-# INLINE groupOpacity #-}
  groupOpacity = drawAttributes . groupOpacity

  markerEnd :: Lens' a (Last ElementRef)
  {-# INLINE markerEnd #-}
  markerEnd = drawAttributes . markerEnd

  markerMid :: Lens' a (Last ElementRef)
  {-# INLINE markerMid #-}
  markerMid = drawAttributes . markerMid

  markerStart :: Lens' a (Last ElementRef)
  {-# INLINE markerStart #-}
  markerStart = drawAttributes . markerStart

  maskRef :: Lens' a (Last ElementRef)
  {-# INLINE maskRef #-}
  maskRef = drawAttributes . maskRef

  strokeColor :: Lens' a (Last Texture)
  {-# INLINE strokeColor #-}
  strokeColor = drawAttributes . strokeColor

  strokeDashArray :: Lens' a (Last [Number])
  {-# INLINE strokeDashArray #-}
  strokeDashArray = drawAttributes . strokeDashArray

  strokeLineCap :: Lens' a (Last Cap)
  {-# INLINE strokeLineCap #-}
  strokeLineCap = drawAttributes . strokeLineCap

  strokeLineJoin :: Lens' a (Last LineJoin)
  {-# INLINE strokeLineJoin #-}
  strokeLineJoin = drawAttributes . strokeLineJoin

  strokeMiterLimit :: Lens' a (Last Double)
  {-# INLINE strokeMiterLimit #-}
  strokeMiterLimit = drawAttributes . strokeMiterLimit

  strokeOffset :: Lens' a (Last Number)
  {-# INLINE strokeOffset #-}
  strokeOffset = drawAttributes . strokeOffset

  strokeOpacity :: Lens' a (Maybe Float)
  {-# INLINE strokeOpacity #-}
  strokeOpacity = drawAttributes . strokeOpacity

  strokeWidth :: Lens' a (Last Number)
  {-# INLINE strokeWidth #-}
  strokeWidth = drawAttributes . strokeWidth

  textAnchor :: Lens' a (Last TextAnchor)
  {-# INLINE textAnchor #-}
  textAnchor = drawAttributes . textAnchor

  transform :: Lens' a (Maybe [Transformation])
  {-# INLINE transform #-}
  transform = drawAttributes . transform

instance HasDrawAttributes DrawAttributes where
  {-# INLINE attrId #-}
  {-# INLINE clipPathRef #-}
  {-# INLINE clipRule #-}
  {-# INLINE fillColor #-}
  {-# INLINE fillOpacity #-}
  {-# INLINE fillRule #-}
  {-# INLINE fontFamily #-}
  {-# INLINE fontSize #-}
  {-# INLINE fontStyle #-}
  {-# INLINE groupOpacity #-}
  {-# INLINE markerEnd #-}
  {-# INLINE markerMid #-}
  {-# INLINE markerStart #-}
  {-# INLINE maskRef #-}
  {-# INLINE strokeColor #-}
  {-# INLINE strokeDashArray #-}
  {-# INLINE strokeLineCap #-}
  {-# INLINE strokeLineJoin #-}
  {-# INLINE strokeMiterLimit #-}
  {-# INLINE strokeOffset #-}
  {-# INLINE strokeOpacity #-}
  {-# INLINE strokeWidth #-}
  {-# INLINE textAnchor #-}
  {-# INLINE transform #-}
  drawAttributes = id

  {-# INLINE attrClass #-}
  attrClass f attr =
    fmap (\y -> attr { _attrClass = y }) (f (_attrClass attr))
  attrId f attr =
    fmap (\y -> attr { _attrId = y }) (f $ _attrId attr)
  clipPathRef f attr =
    fmap (\y -> attr { _clipPathRef = y }) (f $ _clipPathRef attr)
  clipRule f attr =
    fmap (\y -> attr { _clipRule = y }) (f $ _clipRule attr)
  fillColor f attr =
    fmap (\y -> attr { _fillColor = y }) (f $ _fillColor attr)
  fillOpacity f attr =
    fmap (\y -> attr { _fillOpacity = y }) (f $ _fillOpacity attr)
  fillRule f attr =
    fmap (\y -> attr { _fillRule = y }) (f $ _fillRule attr)
  fontFamily f attr =
    fmap (\y -> attr { _fontFamily = y }) (f $ _fontFamily attr)
  fontSize f attr =
    fmap (\y -> attr { _fontSize = y }) (f $ _fontSize attr)
  fontStyle f attr =
    fmap (\y -> attr { _fontStyle = y }) (f $ _fontStyle attr)
  groupOpacity f attr =
    fmap (\y -> attr { _groupOpacity = y }) (f $ _groupOpacity attr)
  markerEnd f attr =
    fmap (\y -> attr { _markerEnd = y }) (f $ _markerEnd attr)
  markerMid f attr =
    fmap (\y -> attr { _markerMid = y }) (f $ _markerMid attr)
  markerStart f attr =
    fmap (\y -> attr { _markerStart = y }) (f $ _markerStart attr)
  maskRef f attr =
    fmap (\y -> attr { _maskRef = y }) (f $ _maskRef attr)
  strokeColor f attr =
    fmap (\y -> attr { _strokeColor = y }) (f $ _strokeColor attr)
  strokeDashArray f attr =
    fmap (\y -> attr { _strokeDashArray = y }) (f $ _strokeDashArray attr)
  strokeLineCap f attr =
    fmap (\y -> attr { _strokeLineCap = y }) (f $ _strokeLineCap attr)
  strokeLineJoin f attr =
    fmap (\y -> attr { _strokeLineJoin = y }) (f $ _strokeLineJoin attr)
  strokeMiterLimit f attr =
    fmap (\y -> attr { _strokeMiterLimit = y }) (f $ _strokeMiterLimit attr)
  strokeOffset f attr =
    fmap (\y -> attr { _strokeOffset = y }) (f $ _strokeOffset attr)
  strokeOpacity f attr =
    fmap (\y -> attr { _strokeOpacity = y }) (f $ _strokeOpacity attr)
  strokeWidth f attr =
    fmap (\y -> attr { _strokeWidth = y }) (f $ _strokeWidth attr)
  textAnchor f attr =
    fmap (\y -> attr { _textAnchor = y }) (f $ _textAnchor attr)
  transform f attr =
    fmap (\y -> attr { _transform = y }) (f $ _transform attr)
