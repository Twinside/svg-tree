module Graphics.Svg.RenderContext where

import Control.Monad.Trans.State.Strict( StateT )
import Control.Applicative( (<$>), pure )
import Codec.Picture( PixelRGBA8( .. ) )
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid( Last( .. ) )

import Graphics.Rasterific.Linear( (^-^) )
import Graphics.Rasterific hiding ( Path, Line, transform )
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType
import Graphics.Svg.Types

toRadian :: Float -> Float
toRadian v = v / 180 * pi

data RenderContext = RenderContext
    { _initialViewBox :: (Point, Point)
    , _renderViewBox  :: (Point, Point)
    , _definitions    :: M.Map String SvgElement
    , _fontCache      :: FontCache
    }

type LoadedFonts = M.Map FilePath Font

type IODraw = StateT LoadedFonts IO

type ViewBox = (Point, Point)

capOfSvg :: SvgDrawAttributes -> (Cap, Cap)
capOfSvg attrs =
  case getLast $ _strokeLineCap attrs of
    Nothing -> (CapStraight 1, CapStraight 1)
    Just SvgCapSquare -> (CapStraight 1, CapStraight 1)
    Just SvgCapButt -> (CapStraight 0, CapStraight 0)
    Just SvgCapRound -> (CapRound, CapRound)


joinOfSvg :: SvgDrawAttributes -> Join
joinOfSvg attrs =
  case (getLast $ _strokeLineJoin attrs, getLast $ _strokeMiterLimit attrs) of
    (Nothing, _) -> JoinRound
    (Just SvgJoinMiter, Just _) -> JoinMiter 0
    (Just SvgJoinMiter, Nothing) -> JoinMiter 0
    (Just SvgJoinBevel, _) -> JoinMiter 5
    (Just SvgJoinRound, _) -> JoinRound

boundingBoxLength :: SvgDrawAttributes -> PlaneBound -> SvgNumber
                  -> Float
boundingBoxLength attr (PlaneBound mini maxi) num =
    case num of
      SvgNum n -> n
      SvgEm n -> emTransform attr n
      SvgPercent p -> p * coeff
  where
     V2 actualWidth actualHeight =
                abs <$> (maxi ^-^ mini)
     two = 2 :: Int
     coeff = sqrt (actualWidth ^^ two + actualHeight ^^ two)
           / sqrt 2

boundbingBoxLinearise :: SvgDrawAttributes -> PlaneBound -> SvgPoint
                      -> Point
boundbingBoxLinearise
    attr (PlaneBound mini@(V2 xi yi) maxi) (xp, yp) = V2 finalX finalY
  where
    V2 w h = abs <$> (maxi ^-^ mini)
    finalX = case xp of
      SvgNum n -> n
      SvgEm n -> emTransform attr n
      SvgPercent p -> p * w + xi

    finalY = case yp of
      SvgNum n -> n
      SvgEm n -> emTransform attr n
      SvgPercent p -> p * h + yi

lineariseXLength :: RenderContext -> SvgDrawAttributes -> SvgNumber
                 -> Coord
lineariseXLength _ _ (SvgNum i) = i
lineariseXLength _ attr (SvgEm i) = emTransform attr i
lineariseXLength ctxt _ (SvgPercent p) = abs (xe - xs) * p
  where
    (V2 xs _, V2 xe _) = _renderViewBox ctxt

lineariseYLength :: RenderContext -> SvgDrawAttributes -> SvgNumber
                 -> Coord
lineariseYLength _ _ (SvgNum i) = i
lineariseYLength _ attr (SvgEm n) = emTransform attr n
lineariseYLength ctxt _ (SvgPercent p) = abs (ye - ys) * p
  where
    (V2 _ ys, V2 _ ye) = _renderViewBox ctxt


linearisePoint :: RenderContext -> SvgDrawAttributes -> SvgPoint
               -> Point
linearisePoint ctxt attr (p1, p2) =
    V2 (xs + lineariseXLength ctxt attr p1)
       (ys + lineariseYLength ctxt attr p2)
  where (V2 xs ys, _) = _renderViewBox ctxt

emTransform :: SvgDrawAttributes -> Float -> Float
emTransform attr n = case getLast $ _fontSize attr of
    Nothing -> 16 * n
    Just (SvgNum v) -> v * n
    Just _ -> 16 * n

lineariseLength :: RenderContext -> SvgDrawAttributes -> SvgNumber
                -> Coord
lineariseLength _ _ (SvgNum i) = i
lineariseLength _ attr (SvgEm i) =
    emTransform attr i
lineariseLength ctxt _ (SvgPercent v) = v * coeff
  where
    (V2 x1 y1, V2 x2 y2) = _renderViewBox ctxt
    actualWidth = abs $ x2 - x1
    actualHeight = abs $ y2 - y1
    two = 2 :: Int
    coeff = sqrt (actualWidth ^^ two + actualHeight ^^ two)
          / sqrt 2

prepareLinearGradientTexture
    :: RenderContext -> SvgDrawAttributes
    -> SvgLinearGradient -> Float -> [Primitive]
    -> Texture PixelRGBA8
prepareLinearGradientTexture ctxt attr grad opa prims =
  let bounds = F.foldMap planeBounds prims
      lineariser = case _linearGradientUnits grad of
        GradientUserSpace -> linearisePoint ctxt attr
        GradientBoundingBox -> boundbingBoxLinearise attr bounds
      gradient =
        [(offset, fillAlphaCombine opa color)
            | SvgGradientStop offset color <- _linearGradientStops grad]
      startPoint = lineariser $ _linearGradientStart grad
      stopPoint = lineariser $ _linearGradientStop grad
  in
  linearGradientTexture gradient startPoint stopPoint

prepareRadialGradientTexture
    :: RenderContext -> SvgDrawAttributes
    -> SvgRadialGradient -> Float -> [Primitive]
    -> Texture PixelRGBA8
prepareRadialGradientTexture ctxt attr grad opa prims =
  let bounds = F.foldMap planeBounds prims
      (lineariser, lengthLinearise) = case _radialGradientUnits grad of
        GradientUserSpace ->
          (linearisePoint ctxt attr, lineariseLength ctxt attr)
        GradientBoundingBox ->
          (boundbingBoxLinearise attr bounds, boundingBoxLength attr bounds)
      gradient =
        [(offset, fillAlphaCombine opa color)
            | SvgGradientStop offset color <- _radialGradientStops grad]
      center = lineariser $ _radialGradientCenter grad
      radius = lengthLinearise $ _radialGradientRadius grad
  in
  case (_radialGradientFocusX grad,
            _radialGradientFocusY grad) of
    (Nothing, Nothing) ->
      radialGradientTexture gradient center radius
    (Just fx, Nothing) ->
      radialGradientWithFocusTexture gradient center radius
        $ lineariser (fx, snd $ _radialGradientCenter grad)
    (Nothing, Just fy) ->
      radialGradientWithFocusTexture gradient center radius
        $ lineariser (fst $ _radialGradientCenter grad, fy)
    (Just fx, Just fy) ->
      radialGradientWithFocusTexture gradient center radius
        $ lineariser (fx, fy)

fillMethodOfSvg :: SvgDrawAttributes -> FillMethod
fillMethodOfSvg attr = case getLast $ _fillRule attr of
    Nothing -> FillWinding
    Just SvgFillNonZero -> FillWinding
    Just SvgFillEvenOdd -> FillEvenOdd

fillAlphaCombine :: Float -> PixelRGBA8 -> PixelRGBA8
fillAlphaCombine opacity (PixelRGBA8 r g b a) =
    PixelRGBA8 r g b alpha
  where
    a' = fromIntegral a / 255.0
    alpha = floor . max 0 . min 255 $ opacity * a' * 255

prepareTexture :: RenderContext -> SvgDrawAttributes
               -> SvgTexture -> Float
               -> [Primitive]
               -> Maybe (Texture PixelRGBA8)
prepareTexture _ _ FillNone _opacity _ = Nothing
prepareTexture _ _ (ColorRef color) opacity _ =
  pure . uniformTexture $ fillAlphaCombine opacity color
prepareTexture ctxt attr (TextureRef ref) opacity prims =
    M.lookup ref (_definitions ctxt) >>= prepare 
  where
    prepare (ElementGeometry _) = Nothing
    prepare (ElementLinearGradient grad) =
        pure $ prepareLinearGradientTexture ctxt 
                        attr grad opacity prims
    prepare (ElementRadialGradient grad) =
        pure $ prepareRadialGradientTexture ctxt
                        attr grad opacity prims

