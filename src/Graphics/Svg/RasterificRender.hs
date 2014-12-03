{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Svg.RasterificRender( renderSvgDocument ) where

import Data.Monoid( Last( .. ), mempty, (<>) )
import Data.Maybe( fromMaybe  )
import Control.Monad( foldM )
import Control.Monad.Trans.State.Strict( runStateT )
import Control.Applicative( (<$>) )
import Codec.Picture( Image, PixelRGBA8( .. ) )
import qualified Data.Foldable as F
import qualified Graphics.Rasterific as R
import Graphics.Rasterific.Linear( V2( V2 ), (^-^), zero )
import Graphics.Rasterific.Outline
import qualified Graphics.Rasterific.Transformations as RT
import Graphics.Text.TrueType
import Graphics.Svg.PathConverter
import Graphics.Svg.Types
import Graphics.Svg.RenderContext
import Graphics.Svg.RasterificTextRendering

{-import Debug.Trace-}
{-import Text.Printf-}
{-import Text.Groom-}


renderSvgDocument :: FontCache -> Maybe (Int, Int) -> Document
                  -> IO (Image PixelRGBA8, LoadedFonts)
renderSvgDocument cache sizes doc = case sizes of
    Just s -> renderAtSize s
    Nothing -> renderAtSize $ documentSize doc
  where
    (x1, y1, x2, y2) = case (_viewBox doc, _width doc, _height doc) of
        (Just v,      _,      _) -> v
        (     _, Just (Num w), Just (Num h)) -> (0, 0, floor w, floor h)
        _                        -> (0, 0, 1, 1)

    box = (V2 (fromIntegral x1) (fromIntegral y1),
           V2 (fromIntegral x2) (fromIntegral y2))
    emptyContext = RenderContext
        { _renderViewBox = box
        , _initialViewBox = box
        , _contextDefinitions = _definitions doc
        , _fontCache = cache
        , _subRender = renderSvgDocument cache Nothing
        }
    white = PixelRGBA8 255 255 255 255

    sizeFitter (V2 0 0, V2 vw vh) (actualWidth, actualHeight)
      | aw /= vw || vh /= ah =
            R.withTransformation (RT.scale (aw / vw) (ah / vh))
           where
             aw = fromIntegral actualWidth
             ah = fromIntegral actualHeight
    sizeFitter (V2 0 0, _) _ = id
    sizeFitter (p@(V2 xs ys), V2 xEnd yEnd) actualSize =
        R.withTransformation (RT.translate (negate p)) .
            sizeFitter (zero, V2 (xEnd - xs) (yEnd - ys)) actualSize

    elements = _elements doc
    renderAtSize (w, h) = do
      let stateDraw = mapM (renderSvg emptyContext) elements
      (elems, s) <- runStateT stateDraw mempty
      let drawing = sizeFitter box (w, h) $ sequence_ elems
          img = R.renderDrawing w h white drawing
      return (img, s)

withInfo :: (Monad m, Monad m2)
         => (a -> Maybe b) -> a -> (b -> m (m2 ())) -> m (m2 ())
withInfo accessor val action =
    case accessor val of
       Nothing -> return $ return ()
       Just v -> action v

toTransformationMatrix :: Transformation -> RT.Transformation
toTransformationMatrix = go where
  go (TransformMatrix t) = t
  go (Translate x y) = RT.translate $ V2 x y
  go (Scale xs Nothing) = RT.scale xs xs
  go (Scale xs (Just ys)) = RT.scale xs ys
  go (Rotate angle Nothing) =
      RT.rotate $ toRadian angle
  go (Rotate angle (Just (cx, cy))) =
      RT.rotateCenter (toRadian angle) $ V2 cx cy
  go (SkewX v) = RT.skewX $ toRadian v
  go (SkewY v) = RT.skewY $ toRadian v
  go TransformUnknown = mempty

withTransform :: DrawAttributes -> R.Drawing a ()
              -> R.Drawing a ()
withTransform trans draw =
    case _transform trans of
       Nothing -> draw
       Just t -> R.withTransformation fullTrans draw
         where fullTrans = F.foldMap toTransformationMatrix t

withSvgTexture :: RenderContext -> DrawAttributes
               -> Texture -> Float
               -> [R.Primitive]
               -> IODraw (R.Drawing PixelRGBA8 ())
withSvgTexture ctxt attr texture opacity prims = do
  mayTexture <- prepareTexture ctxt attr texture opacity prims
  case mayTexture of
    Nothing -> return $ return ()
    Just tex ->
      let method = fillMethodOfSvg attr in
      return . R.withTexture tex $ R.fillWithMethod method prims

filler :: RenderContext
       -> DrawAttributes
       -> [R.Primitive]
       -> IODraw (R.Drawing PixelRGBA8 ())
filler ctxt info primitives =
  withInfo (getLast . _fillColor) info $ \svgTexture ->
    let opacity = fromMaybe 1.0 $ _fillOpacity info in
    withSvgTexture ctxt info svgTexture opacity primitives

stroker :: RenderContext -> DrawAttributes -> [R.Primitive]
        -> IODraw (R.Drawing PixelRGBA8 ())
stroker ctxt info primitives =
  withInfo (getLast . _strokeWidth) info $ \swidth ->
    withInfo (getLast . _strokeColor) info $ \svgTexture ->
      let toFloat = lineariseLength ctxt info
          realWidth = toFloat swidth
          dashOffsetStart =
              maybe 0 toFloat . getLast $ _strokeOffset info
          primsList = case getLast $ _strokeDashArray info of
            Just pat ->
                dashedStrokize dashOffsetStart (toFloat <$> pat)
                  realWidth (joinOfSvg info) (capOfSvg info) primitives
            Nothing ->
              [strokize realWidth (joinOfSvg info) (capOfSvg info) primitives]
          opacity = fromMaybe 1.0 $ _strokeOpacity info
          strokerAction acc prims =
           (acc <>) <$>
               withSvgTexture ctxt info svgTexture opacity prims
            
      in
      foldM strokerAction mempty primsList

mergeContext :: RenderContext -> DrawAttributes -> RenderContext
mergeContext ctxt _attr = ctxt

viewBoxOfTree :: Tree -> Maybe (Int, Int, Int, Int)
viewBoxOfTree (SymbolTree g) = _groupViewBox g
viewBoxOfTree _ = Nothing

renderSvg :: RenderContext -> Tree -> IODraw (R.Drawing PixelRGBA8 ())
renderSvg initialContext = go initialContext initialAttr
  where
    initialAttr =
      mempty { _strokeWidth = Last . Just $ Num 1.0
             , _strokeLineCap = Last $ Just CapButt
             , _strokeLineJoin = Last $ Just JoinMiter
             , _strokeMiterLimit = Last $ Just 4.0
             , _strokeOpacity = Just 1.0
             , _fillColor = Last . Just . ColorRef $ PixelRGBA8 0 0 0 255
             , _fillOpacity = Just 1.0
             , _fillRule = Last $ Just FillNonZero
             , _fontSize = Last . Just $ Num 12
             , _fontFamily = Last $ Just ["Verdana"]
             , _textAnchor = Last $ Just TextAnchorStart
             }

    fitUse ctxt attr uses subTree =
      let origin = linearisePoint ctxt attr $ _useBase uses
          w = lineariseXLength ctxt attr <$> _useWidth uses
          h = lineariseYLength ctxt attr <$> _useHeight uses
      in
      case viewBoxOfTree subTree of
        Nothing -> R.withTransformation (RT.translate origin)
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
          R.withTransformation $ RT.translate origin
                              <> RT.scale xScaleFactor yScaleFactor
                              <> RT.translate (negate boxOrigin)


    go _ _ None = return mempty
    go ctxt attr (TextArea tp stext) = renderText ctxt attr tp stext

    go ctxt attr (UseTree useData subTree) = do
      sub <- go ctxt attr' subTree
      return . fitUse ctxt attr useData subTree
             $ withTransform pAttr sub
      where
        pAttr = _useDrawAttributes useData
        attr' = attr <> pAttr

    go ctxt attr (SymbolTree g) = go ctxt attr $ GroupTree g
    go ctxt attr (GroupTree (Group groupAttr subTrees _)) = do
        subTrees' <- mapM (go context' attr') subTrees
        return . withTransform groupAttr $ sequence_ subTrees'
      where attr' = attr <> groupAttr
            context' = mergeContext ctxt groupAttr

    go ctxt attr (RectangleTree (Rectangle pAttr p w h (rx, ry))) = do
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

      filling <- filler context' info rect
      stroking <- stroker context' info rect
      return . withTransform pAttr $ filling <> stroking

    go ctxt attr (CircleTree (Circle pAttr p r)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          r' = lineariseLength context' info r
          c = R.circle p' r'
      filling <- filler context' info c
      stroking <- stroker context' info c
      return . withTransform pAttr $ filling <> stroking

    go ctxt attr (EllipseTree (Ellipse pAttr p rx ry)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          rx' = lineariseXLength context' info rx
          ry' = lineariseYLength context' info ry
          c = R.ellipse p' rx' ry'
      filling <- filler context' info c
      stroking <- stroker context' info c
      return . withTransform pAttr $ filling <> stroking

    go ctxt attr (PolyLineTree (PolyLine pAttr points)) =
      go ctxt (dropFillColor attr)
            . Path . PathPrim (dropFillColor pAttr)
            $ toPath points
      where
        dropFillColor v = v { _fillColor = Last Nothing }
        toPath [] = []
        toPath (x:xs) =
            [ MoveTo OriginAbsolute [x]
            , LineTo OriginAbsolute xs
            ]

    go ctxt attr (PolygonTree (Polygon pAttr points)) =
      go ctxt attr . Path . PathPrim pAttr $ toPath points
      where
        toPath [] = []
        toPath (x:xs) =
            [ MoveTo OriginAbsolute [x]
            , LineTo OriginAbsolute xs
            , EndPath
            ]

    go ctxt attr (LineTree (Line pAttr p1 p2)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p1' = linearisePoint context' info p1
          p2' = linearisePoint context' info p2
      stroking <- stroker context' info $ R.line p1' p2'
      return $ withTransform pAttr stroking

    go ctxt attr (Path (PathPrim pAttr path)) = do
      let info = attr <> pAttr
          strokePrimitives = svgPathToPrimitives False path
          fillPrimitives = svgPathToPrimitives True path
      filling <- filler ctxt info fillPrimitives
      stroking <- stroker ctxt info strokePrimitives
      return . withTransform pAttr $ filling <> stroking

