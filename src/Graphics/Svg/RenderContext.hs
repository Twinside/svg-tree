module Graphics.Svg.RenderContext where

import Control.Monad.Trans.State.Strict( StateT )
import Control.Applicative( (<$>), pure )
import Codec.Picture( PixelRGBA8( .. ) )
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid( Last( .. ) )

import Graphics.Rasterific.Linear( (^-^) )
import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Texture as RT
import Graphics.Text.TrueType
import Graphics.Svg.Types

toRadian :: Float -> Float
toRadian v = v / 180 * pi

data RenderContext = RenderContext
    { _initialViewBox     :: (R.Point, R.Point)
    , _renderViewBox      :: (R.Point, R.Point)
    , _contextDefinitions :: M.Map String Element
    , _fontCache          :: FontCache
    }

type LoadedFonts = M.Map FilePath Font

type IODraw = StateT LoadedFonts IO

type ViewBox = (R.Point, R.Point)

capOfSvg :: DrawAttributes -> (R.Cap, R.Cap)
capOfSvg attrs =
  case getLast $ _strokeLineCap attrs of
    Nothing -> (R.CapStraight 1, R.CapStraight 1)
    Just CapSquare -> (R.CapStraight 1, R.CapStraight 1)
    Just CapButt -> (R.CapStraight 0, R.CapStraight 0)
    Just CapRound -> (R.CapRound, R.CapRound)


joinOfSvg :: DrawAttributes -> R.Join
joinOfSvg attrs =
  case (getLast $ _strokeLineJoin attrs, getLast $ _strokeMiterLimit attrs) of
    (Nothing, _) -> R.JoinRound
    (Just JoinMiter, Just _) -> R.JoinMiter 0
    (Just JoinMiter, Nothing) -> R.JoinMiter 0
    (Just JoinBevel, _) -> R.JoinMiter 5
    (Just JoinRound, _) -> R.JoinRound

boundingBoxLength :: DrawAttributes -> R.PlaneBound -> Number
                  -> Float
boundingBoxLength attr (R.PlaneBound mini maxi) num =
    case num of
      Num n -> n
      Em n -> emTransform attr n
      Percent p -> p * coeff
  where
     R.V2 actualWidth actualHeight =
                abs <$> (maxi ^-^ mini)
     two = 2 :: Int
     coeff = sqrt (actualWidth ^^ two + actualHeight ^^ two)
           / sqrt 2

boundbingBoxLinearise :: DrawAttributes -> R.PlaneBound -> Point
                      -> R.Point
boundbingBoxLinearise
    attr (R.PlaneBound mini@(R.V2 xi yi) maxi) (xp, yp) = R.V2 finalX finalY
  where
    R.V2 w h = abs <$> (maxi ^-^ mini)
    finalX = case xp of
      Num n -> n
      Em n -> emTransform attr n
      Percent p -> p * w + xi

    finalY = case yp of
      Num n -> n
      Em n -> emTransform attr n
      Percent p -> p * h + yi

lineariseXLength :: RenderContext -> DrawAttributes -> Number
                 -> Coord
lineariseXLength _ _ (Num i) = i
lineariseXLength _ attr (Em i) = emTransform attr i
lineariseXLength ctxt _ (Percent p) = abs (xe - xs) * p
  where
    (R.V2 xs _, R.V2 xe _) = _renderViewBox ctxt

lineariseYLength :: RenderContext -> DrawAttributes -> Number
                 -> Coord
lineariseYLength _ _ (Num i) = i
lineariseYLength _ attr (Em n) = emTransform attr n
lineariseYLength ctxt _ (Percent p) = abs (ye - ys) * p
  where
    (R.V2 _ ys, R.V2 _ ye) = _renderViewBox ctxt


linearisePoint :: RenderContext -> DrawAttributes -> Point
               -> R.Point
linearisePoint ctxt attr (p1, p2) =
  R.V2 (xs + lineariseXLength ctxt attr p1)
       (ys + lineariseYLength ctxt attr p2)
  where (R.V2 xs ys, _) = _renderViewBox ctxt

emTransform :: DrawAttributes -> Float -> Float
emTransform attr n = case getLast $ _fontSize attr of
    Nothing -> 16 * n
    Just (Num v) -> v * n
    Just _ -> 16 * n

lineariseLength :: RenderContext -> DrawAttributes -> Number
                -> Coord
lineariseLength _ _ (Num i) = i
lineariseLength _ attr (Em i) =
    emTransform attr i
lineariseLength ctxt _ (Percent v) = v * coeff
  where
    (R.V2 x1 y1, R.V2 x2 y2) = _renderViewBox ctxt
    actualWidth = abs $ x2 - x1
    actualHeight = abs $ y2 - y1
    two = 2 :: Int
    coeff = sqrt (actualWidth ^^ two + actualHeight ^^ two)
          / sqrt 2

prepareLinearGradientTexture
    :: RenderContext -> DrawAttributes
    -> LinearGradient -> Float -> [R.Primitive]
    -> R.Texture PixelRGBA8
prepareLinearGradientTexture ctxt attr grad opa prims =
  let bounds = F.foldMap R.planeBounds prims
      lineariser = case _linearGradientUnits grad of
        GradientUserSpace -> linearisePoint ctxt attr
        GradientBoundingBox -> boundbingBoxLinearise attr bounds
      gradient =
        [(offset, fillAlphaCombine opa color)
            | GradientStop offset color <- _linearGradientStops grad]
      startPoint = lineariser $ _linearGradientStart grad
      stopPoint = lineariser $ _linearGradientStop grad
  in
  RT.linearGradientTexture gradient startPoint stopPoint

prepareRadialGradientTexture
    :: RenderContext -> DrawAttributes
    -> RadialGradient -> Float -> [R.Primitive]
    -> R.Texture PixelRGBA8
prepareRadialGradientTexture ctxt attr grad opa prims =
  let bounds = F.foldMap R.planeBounds prims
      (lineariser, lengthLinearise) = case _radialGradientUnits grad of
        GradientUserSpace ->
          (linearisePoint ctxt attr, lineariseLength ctxt attr)
        GradientBoundingBox ->
          (boundbingBoxLinearise attr bounds, boundingBoxLength attr bounds)
      gradient =
        [(offset, fillAlphaCombine opa color)
            | GradientStop offset color <- _radialGradientStops grad]
      center = lineariser $ _radialGradientCenter grad
      radius = lengthLinearise $ _radialGradientRadius grad
  in
  case (_radialGradientFocusX grad,
            _radialGradientFocusY grad) of
    (Nothing, Nothing) ->
      RT.radialGradientTexture gradient center radius
    (Just fx, Nothing) ->
      RT.radialGradientWithFocusTexture gradient center radius
        $ lineariser (fx, snd $ _radialGradientCenter grad)
    (Nothing, Just fy) ->
      RT.radialGradientWithFocusTexture gradient center radius
        $ lineariser (fst $ _radialGradientCenter grad, fy)
    (Just fx, Just fy) ->
      RT.radialGradientWithFocusTexture gradient center radius
        $ lineariser (fx, fy)

fillMethodOfSvg :: DrawAttributes -> R.FillMethod
fillMethodOfSvg attr = case getLast $ _fillRule attr of
    Nothing -> R.FillWinding
    Just FillNonZero -> R.FillWinding
    Just FillEvenOdd -> R.FillEvenOdd

fillAlphaCombine :: Float -> PixelRGBA8 -> PixelRGBA8
fillAlphaCombine opacity (PixelRGBA8 r g b a) =
    PixelRGBA8 r g b alpha
  where
    a' = fromIntegral a / 255.0
    alpha = floor . max 0 . min 255 $ opacity * a' * 255

prepareTexture :: RenderContext -> DrawAttributes
               -> Texture -> Float
               -> [R.Primitive]
               -> Maybe (R.Texture PixelRGBA8)
prepareTexture _ _ FillNone _opacity _ = Nothing
prepareTexture _ _ (ColorRef color) opacity _ =
  pure . RT.uniformTexture $ fillAlphaCombine opacity color
prepareTexture ctxt attr (TextureRef ref) opacity prims =
    M.lookup ref (_contextDefinitions ctxt) >>= prepare 
  where
    prepare (ElementGeometry _) = Nothing
    prepare (ElementLinearGradient grad) =
        pure $ prepareLinearGradientTexture ctxt 
                        attr grad opacity prims
    prepare (ElementRadialGradient grad) =
        pure $ prepareRadialGradientTexture ctxt
                        attr grad opacity prims

