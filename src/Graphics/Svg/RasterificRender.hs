{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Svg.RasterificRender where

import Control.Monad.Trans.State.Strict( runStateT )
import Control.Applicative( (<$>) )
import Codec.Picture( Image, PixelRGBA8( .. ) )
import qualified Data.Foldable as F
import Data.Monoid( mempty, (<>), Last( .. ) )
import qualified Graphics.Rasterific as R
import Graphics.Rasterific.Linear( V2( V2 ), (^-^), zero )
import Graphics.Rasterific.Outline
import Graphics.Rasterific.Transformations
import Graphics.Text.TrueType
import Graphics.Svg.PathConverter
import Graphics.Svg.Types
import Graphics.Svg.RenderContext
import Graphics.Svg.RasterificTextRendering

{-import Debug.Trace-}
{-import Text.Printf-}
{-import Text.Groom-}

capOfSvg :: SvgDrawAttributes -> (R.Cap, R.Cap)
capOfSvg attrs =
  case getLast $ _strokeLineCap attrs of
    Nothing -> (R.CapStraight 1, R.CapStraight 1)
    Just SvgCapSquare -> (R.CapStraight 1, R.CapStraight 1)
    Just SvgCapButt -> (R.CapStraight 0, R.CapStraight 0)
    Just SvgCapRound -> (R.CapRound, R.CapRound)


joinOfSvg :: SvgDrawAttributes -> R.Join
joinOfSvg attrs =
  case (getLast $ _strokeLineJoin attrs, getLast $ _strokeMiterLimit attrs) of
    (Nothing, _) -> R.JoinRound
    (Just SvgJoinMiter, Just _) -> R.JoinMiter 0
    (Just SvgJoinMiter, Nothing) -> R.JoinMiter 0
    (Just SvgJoinBevel, _) -> R.JoinMiter 5
    (Just SvgJoinRound, _) -> R.JoinRound


renderSvgDocument :: FontCache -> Maybe (Int, Int) -> SvgDocument
                  -> IO (Image PixelRGBA8, LoadedFonts)
renderSvgDocument cache sizes doc = case sizes of
    Just s -> renderAtSize s
    Nothing -> renderAtSize $ svgDocumentSize doc
  where
    (x1, y1, x2, y2) = case (_svgViewBox doc, _svgWidth doc, _svgHeight doc) of
        (Just v,      _,      _) -> v
        (     _, Just (SvgNum w), Just (SvgNum h)) -> (0, 0, floor w, floor h)
        _                        -> (0, 0, 1, 1)

    box = (V2 (fromIntegral x1) (fromIntegral y1),
           V2 (fromIntegral x2) (fromIntegral y2))
    emptyContext = RenderContext
        { _renderViewBox = box
        , _initialViewBox = box
        , _definitions = _svgDefinitions doc
        , _fontCache = cache
        }
    white = PixelRGBA8 255 255 255 255

    sizeFitter (V2 0 0, V2 vw vh) (actualWidth, actualHeight)
      | aw /= vw || vh /= ah =
            R.withTransformation (scale (aw / vw) (ah / vh))
           where
             aw = fromIntegral actualWidth
             ah = fromIntegral actualHeight
    sizeFitter (V2 0 0, _) _ = id
    sizeFitter (p@(V2 xs ys), V2 xEnd yEnd) actualSize =
        R.withTransformation (translate (negate p)) .
            sizeFitter (zero, V2 (xEnd - xs) (yEnd - ys)) actualSize

    elements = _svgElements doc
    renderAtSize (w, h) = do
      let stateDraw = mapM (renderSvg emptyContext) elements
      (elems, s) <- runStateT stateDraw mempty
      let drawing = sizeFitter box (w, h) $ sequence_ elems
          img = R.renderDrawing w h white drawing
      return (img, s)

withInfo :: Monad m => (a -> Maybe b) -> a -> (b -> m ()) -> m ()
withInfo accessor val action = F.forM_ (accessor val) action

toTransformationMatrix :: SvgTransformation -> Transformation
toTransformationMatrix = go where
  go (SvgTransformMatrix t) = t
  go (SvgTranslate x y) = translate $ V2 x y
  go (SvgScale xs Nothing) = scale xs xs
  go (SvgScale xs (Just ys)) = scale xs ys
  go (SvgRotate angle Nothing) =
      rotate $ toRadian angle
  go (SvgRotate angle (Just (cx, cy))) =
      rotateCenter (toRadian angle) $ V2 cx cy
  go (SvgSkewX v) = skewX $ toRadian v
  go (SvgSkewY v) = skewY $ toRadian v
  go SvgTransformUnknown = mempty

withTransform :: SvgDrawAttributes -> R.Drawing a () -> R.Drawing a ()
withTransform trans draw =
    case _transform trans of
       Nothing -> draw
       Just t -> R.withTransformation fullTrans draw
         where fullTrans = F.foldMap toTransformationMatrix t

withSvgTexture :: RenderContext -> SvgDrawAttributes
               -> SvgTexture -> Float
               -> [R.Primitive]
               -> R.Drawing PixelRGBA8 ()
withSvgTexture ctxt attr texture opacity prims =
  case prepareTexture ctxt attr texture opacity prims of
    Nothing -> return ()
    Just tex ->
      let method = fillMethodOfSvg attr in
      R.withTexture tex $ R.fillWithMethod method prims

filler :: RenderContext
       -> SvgDrawAttributes
       -> [R.Primitive]
       -> R.Drawing PixelRGBA8 ()
filler ctxt info primitives =
  withInfo (getLast . _fillColor) info $ \svgTexture ->
    withSvgTexture ctxt info svgTexture (_fillOpacity info) primitives

stroker :: RenderContext -> SvgDrawAttributes -> [R.Primitive]
        -> R.Drawing PixelRGBA8 ()
stroker ctxt info primitives =
  withInfo (getLast . _strokeWidth) info $ \swidth ->
    withInfo (getLast . _strokeColor) info $ \svgTexture ->
      let toFloat = lineariseLength ctxt info
          realWidth = toFloat swidth
          dashOffsetStart =
              maybe 0 toFloat . getLast $ _strokeOffset info
          primsList = case getLast $ _strokeDashArray info of
            Just pattern ->
                dashedStrokize dashOffsetStart (toFloat <$> pattern)
                  realWidth (joinOfSvg info) (capOfSvg info) primitives
            Nothing ->
              [strokize realWidth (joinOfSvg info) (capOfSvg info) primitives]
      in
      mapM_ (withSvgTexture ctxt info svgTexture (_strokeOpacity info)) primsList

mergeContext :: RenderContext -> SvgDrawAttributes -> RenderContext
mergeContext ctxt _attr = ctxt

viewBoxOfTree :: SvgTree -> Maybe (Int, Int, Int, Int)
viewBoxOfTree (Symbol g) = _svgGroupViewBox g
viewBoxOfTree _ = Nothing

renderSvg :: RenderContext -> SvgTree -> IODraw (R.Drawing PixelRGBA8 ())
renderSvg initialContext = go initialContext initialAttr
  where
    initialAttr =
      mempty { _strokeWidth = Last . Just $ SvgNum 1.0
             , _strokeLineCap = Last $ Just SvgCapButt
             , _strokeLineJoin = Last $ Just SvgJoinMiter
             , _strokeMiterLimit = Last $ Just 4.0
             , _strokeOpacity = 1.0
             , _fillColor = Last . Just . ColorRef $ PixelRGBA8 0 0 0 255
             , _fillOpacity = 1.0
             , _fillRule = Last $ Just SvgFillNonZero
             , _fontSize = Last . Just $ SvgNum 12
             , _fontFamily = Last $ Just ["Verdana"]
             , _textAnchor = Last $ Just SvgTextAnchorStart
             }

    fitUse ctxt attr use subTree =
      let origin = linearisePoint ctxt attr $ _svgUseBase use
          w = lineariseXLength ctxt attr <$> _svgUseWidth use
          h = lineariseYLength ctxt attr <$> _svgUseHeight use
      in
      case viewBoxOfTree subTree of
        Nothing -> R.withTransformation (translate origin)
        (Just (xs, ys, xe, ye)) ->
          let boxOrigin = V2 (fromIntegral xs) (fromIntegral ys)
              boxEnd = V2 (fromIntegral xe) (fromIntegral ye)
              V2 bw bh = abs $ boxEnd ^-^ boxOrigin
              xScaleFactor = case w of
                Just wpx -> wpx / bw
                Nothing -> 1.0
              yScaleFactor = case h of
                Just hpx -> hpx / bh
                Nothing -> 1.0
          in
          R.withTransformation $ translate origin
                              <> scale xScaleFactor yScaleFactor
                              <> translate (negate boxOrigin)


    go _ _ SvgNone = return mempty
    go ctxt attr (TextArea tp stext) = renderText ctxt attr tp stext

    go ctxt attr (Use useData subTree) = do
      sub <- go ctxt attr' subTree
      return . fitUse ctxt attr useData subTree
             $ withTransform pAttr sub
      where
        pAttr = _svgUseDrawAttributes useData
        attr' = attr <> pAttr

    go ctxt attr (Symbol g) = go ctxt attr $ Group g
    go ctxt attr (Group (SvgGroup groupAttr subTrees _)) = do
        subTrees' <- mapM (go context' attr') subTrees
        return . withTransform groupAttr $ sequence_ subTrees'
      where attr' = attr <> groupAttr
            context' = mergeContext ctxt groupAttr

    go ctxt attr (Rectangle (SvgRectangle pAttr p w h (rx, ry))) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          w' = lineariseXLength context' info w
          h' = lineariseYLength context' info h

          rx' = lineariseXLength context' info rx
          ry' = lineariseXLength context' info ry
          rect = case (rx', ry') of
            (0, 0) -> R.rectangle p' w' h'
            (v, 0) -> R.roundedRectangle p' w' h' v v
            (0, v) -> R.roundedRectangle p' w' h' v v
            (vx, vy) -> R.roundedRectangle p' w' h' vx vy

      return . withTransform pAttr $ do
        filler context' info rect
        stroker context' info rect

    go ctxt attr (Circle (SvgCircle pAttr p r)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          r' = lineariseLength context' info r
          c = R.circle p' r'
      return . withTransform pAttr $ do
        filler context' info c
        stroker context' info c

    go ctxt attr (Ellipse (SvgEllipse pAttr p rx ry)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          rx' = lineariseXLength context' info rx
          ry' = lineariseYLength context' info ry
          c = R.ellipse p' rx' ry'
      return . withTransform pAttr $ do
        filler context' info c
        stroker context' info c

    go ctxt attr (PolyLine (SvgPolyLine pAttr points)) =
      go ctxt (dropFillColor attr)
            . Path . SvgPathPrim (dropFillColor pAttr)
            $ toPath points
      where
        dropFillColor v = v { _fillColor = Last Nothing }
        toPath [] = []
        toPath (x:xs) =
            [ MoveTo OriginAbsolute [x]
            , LineTo OriginAbsolute xs
            ]

    go ctxt attr (Polygon (SvgPolygon pAttr points)) =
      go ctxt attr . Path . SvgPathPrim pAttr $ toPath points
      where
        toPath [] = []
        toPath (x:xs) =
            [ MoveTo OriginAbsolute [x]
            , LineTo OriginAbsolute xs
            , EndPath
            ]

    go ctxt attr (Line (SvgLine pAttr p1 p2)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p1' = linearisePoint context' info p1
          p2' = linearisePoint context' info p2
      return . withTransform pAttr . stroker context' info $ R.line p1' p2'

    go ctxt attr (Path (SvgPathPrim pAttr path)) = do
      let info = attr <> pAttr
          strokePrimitives = svgPathToPrimitives False path
          fillPrimitives = svgPathToPrimitives True path
      return . withTransform pAttr $ do
        filler ctxt info fillPrimitives
        stroker ctxt info strokePrimitives

