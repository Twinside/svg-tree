module Graphics.Svg.RasterificRender where

import Control.Applicative( (<$>), pure )
import Codec.Picture( Image, PixelRGBA8( .. ) )
import Data.Maybe( fromMaybe )
import Data.Monoid( mempty, (<>) )
import Data.List( mapAccumL )
import Graphics.Rasterific.Linear( (^+^), (^-^), (^*) )
import Graphics.Rasterific hiding ( Path, Line )
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Graphics.Svg.Types
{-import Graphics.Svg.XmlParser-}

capOfSvg :: SvgDrawAttributes -> (Cap, Cap)
capOfSvg attrs =
  case _strokeLineCap attrs of
    Nothing -> (CapStraight 1, CapStraight 1)
    Just SvgCapSquare -> (CapStraight 1, CapStraight 1)
    Just SvgCapButt -> (CapStraight 0, CapStraight 0)
    Just SvgCapRound -> (CapRound, CapRound)

joinOfSvg :: SvgDrawAttributes -> Join
joinOfSvg attrs =
  case (_strokeLineJoin attrs,_strokeMiterLimit attrs) of
    (Nothing, _) -> JoinRound
    (Just SvgJoinMiter, Just v) -> JoinMiter v
    (Just SvgJoinMiter, Nothing) -> JoinMiter 0
    (Just SvgJoinBevel, _) -> JoinMiter 5
    (Just SvgJoinRound, _) -> JoinRound

singularize :: [SvgPath] -> [SvgPath]
singularize = concatMap go
  where
   go (MoveTo _ []) = []
   go (MoveTo o (x: xs)) = MoveTo o [x] : go (LineTo o xs)
   go (LineTo o lst) = LineTo o . pure <$> lst
   go (HorizontalTo o lst) = HorizontalTo o . pure <$> lst
   go (VerticalTo o lst) = VerticalTo o . pure <$> lst
   go (CurveTo o lst) = CurveTo o . pure <$> lst
   go (SmoothCurveTo o lst) = SmoothCurveTo o . pure <$> lst
   go (QuadraticBezier o lst) = QuadraticBezier o . pure <$> lst
   go (SmoothQuadraticBezierCurveTo o lst) =
       SmoothQuadraticBezierCurveTo o . pure <$> lst
   go (ElipticalArc o lst) = ElipticalArc o . pure <$> lst
   go EndPath = [EndPath]

svgPathToPrimitives :: [SvgPath] -> [Primitive]
svgPathToPrimitives lst =
    concat . snd . mapAccumL go (zero, zero, zero)
           $ singularize lst
  where
    zero = V2 0 0

    go o@(lastPoint, _, firstPoint) EndPath =
        (o, line lastPoint firstPoint)

    go o (HorizontalTo _ []) = (o, [])
    go o (VerticalTo _ []) = (o, [])
    go o (MoveTo _ []) = (o, [])
    go o (LineTo _ []) = (o, [])
    go o (CurveTo _ []) = (o, [])
    go o (SmoothCurveTo _ []) = (o, [])
    go o (QuadraticBezier _ []) = (o, [])
    go o (SmoothQuadraticBezierCurveTo  _ []) = (o, [])

    go (_, _, _) (MoveTo OriginAbsolute (p:_)) = ((p, p, p), [])
    go (o, _, _) (MoveTo OriginRelative (p:_)) =
        ((pp, pp, pp), []) where pp = o ^+^ p

    go (o@(V2 _ y), _, fp) (HorizontalTo OriginAbsolute (c:_)) =
        ((p, p, fp), line o p) where p = V2 c y
    go (o@(V2 x y), _, fp) (HorizontalTo OriginRelative (c:_)) =
        ((p, p, fp), line o p) where p = V2 (x + c) y

    go (o@(V2 x _), _, fp) (VerticalTo OriginAbsolute (c:_)) =
        ((p, p, fp), line o p) where p = V2 x c
    go (o@(V2 x y), _, fp) (VerticalTo OriginRelative (c:_)) =
        ((p, p, fp), line o p) where p = V2 x (c + y)

    go (o, _, fp) (LineTo OriginRelative (c:_)) =
        ((p, p, fp), line o p) where p = o ^+^ c

    go (o, _, fp) (LineTo OriginAbsolute (p:_)) =
        ((p, p, fp), line o p)

    go (o, _, fp) (CurveTo OriginAbsolute ((c1, c2, e):_)) =
        ((e, c2, fp), [CubicBezierPrim $ CubicBezier o c1 c2 e])

    go (o, _, fp) (CurveTo OriginRelative ((c1, c2, e):_)) =
        ((e', c2', fp), [CubicBezierPrim $ CubicBezier o c1' c2' e'])
      where c1' = o ^+^ c1
            c2' = o ^+^ c2
            e' = o ^+^ e

    go (o, control, fp) (SmoothCurveTo OriginAbsolute ((c2, e):_)) =
        ((e, c2, fp), [CubicBezierPrim $ CubicBezier o c1' c2 e])
      where c1' = o ^* 2 ^-^ control

    go (o, control, fp) (SmoothCurveTo OriginRelative ((c2, e):_)) =
        ((e', c2', fp), [CubicBezierPrim $ CubicBezier o c1' c2' e'])
      where c1' = o ^* 2 ^-^ control
            c2' = o ^+^ c2
            e' = o ^+^ e

    go (o, _, fp) (QuadraticBezier OriginAbsolute ((c1, e):_)) =
        ((e, c1, fp), [BezierPrim $ Bezier o c1 e])

    go (o, _, fp) (QuadraticBezier OriginRelative ((c1, e):_)) =
        ((e', c1', fp), [BezierPrim $ Bezier o c1' e'])
      where c1' = o ^+^ c1
            e' = o ^+^ e

    go (o, control, fp)
       (SmoothQuadraticBezierCurveTo OriginAbsolute (e:_)) =
       ((e, c1', fp), [BezierPrim $ Bezier o c1' e])
      where c1' = o ^* 2 ^-^ control

    go (o, control, fp)
       (SmoothQuadraticBezierCurveTo OriginRelative (e:_)) =
       ((e', c1', fp), [BezierPrim $ Bezier o c1' e'])
      where c1' = o ^* 2 ^-^ control
            e' = o ^+^ e

    go _ (ElipticalArc _ _) = error "Unimplemented"


renderSvgDocument :: Maybe (Int, Int) -> SvgDocument -> Image PixelRGBA8
renderSvgDocument sizes doc = case sizes of
    Just s -> renderAtSize 
    Nothing -> renderAtSize $ svgDocumentSize doc
  where
    (x1, y1, x2, y2) = case _svgViewBox doc of
        Just v -> v
        Nothing ->

    box = (V2 (fromIntegral x1) (fromIntegral y1),
           V2 (fromIntegral x2) (fromIntegral y2))
    emptyContext = RenderContext
        { _renderViewBox = box
        , _initialViewBox = box
        }
    white = PixelRGBA8 255 255 255 255
    renderAtSize (w, h) =
        renderDrawing w h white . mapM_ (renderSvg emptyContext) $ _svgElements doc

withInfo :: Monad m => (a -> Maybe b) -> a -> (b -> m ()) -> m ()
withInfo accessor val action =
  case accessor val of
    Nothing -> return ()
    Just v -> action v

withTransform :: SvgDrawAttributes -> [Primitive] -> [Primitive]
withTransform trans prims =
    case _transform trans of
       Nothing -> prims
       Just t -> transform (applyTransformation t) <$> prims

data RenderContext = RenderContext
    { _initialViewBox :: (Point, Point)
    , _renderViewBox :: (Point, Point)
    }

type ViewBox = (Point, Point)

filler :: SvgDrawAttributes -> [Primitive] -> Drawing PixelRGBA8 ()
filler info primitives =
  withInfo _fillColor info $ \c ->
    withTexture (uniformTexture c) $ fill primitives

stroker :: SvgDrawAttributes -> [Primitive] -> Drawing PixelRGBA8 ()
stroker info primitives =
  withInfo _strokeWidth info $ \swidth ->
    withInfo _strokeColor info $ \color ->
      withTexture (uniformTexture color) $
        stroke swidth (joinOfSvg info) (capOfSvg info) primitives

mergeContext :: RenderContext -> SvgDrawAttributes -> RenderContext
mergeContext ctxt attr = case _transform attr of
  Nothing -> ctxt
  Just v -> ctxt { _renderViewBox = (trans iniMin, trans iniMax) }
    where
      (iniMin, iniMax) = _initialViewBox ctxt
      inv = fromMaybe mempty $ inverseTransformation v
      trans = applyTransformation inv

lineariseXLength :: RenderContext -> SvgNumber -> Coord
lineariseXLength _ (SvgNum i) = i
lineariseXLength ctxt (SvgPercent p) = abs (xe - xs) * p
  where
    (V2 xs _, V2 xe _) = _renderViewBox ctxt

lineariseYLength :: RenderContext -> SvgNumber -> Coord
lineariseYLength _ (SvgNum i) = i
lineariseYLength ctxt (SvgPercent p) = abs (ye - ys) * p
  where
    (V2 _ ys, V2 _ ye) = _renderViewBox ctxt
    

linearisePoint :: RenderContext -> SvgPoint -> Point
linearisePoint ctxt (p1, p2) =
    V2 (xs + lineariseXLength ctxt p1)
       (ys + lineariseYLength ctxt p2)
  where (V2 xs ys, _) = _renderViewBox ctxt

lineariseLength :: RenderContext -> SvgNumber -> Coord
lineariseLength _ (SvgNum i) = i
lineariseLength ctxt (SvgPercent v) = v * coeff
  where
    (V2 x1 y1, V2 x2 y2) = _renderViewBox ctxt
    actualWidth = abs $ x2 - x1
    actualHeight = abs $ y2 - y1
    two = 2 :: Int
    coeff = sqrt (actualWidth ^^ two + actualHeight ^^ two)
          / sqrt 2

renderSvg :: RenderContext -> SvgTree -> Drawing PixelRGBA8 ()
renderSvg initialContext = go initialContext initialAttr
  where
    initialAttr =
      mempty { _strokeWidth = Just 1.0
             , _strokeLineCap = Just SvgCapButt
             , _strokeLineJoin = Just SvgJoinMiter
             , _strokeMiterLimit = Just 4.0
             , _strokeOpacity = Just 1.0
             , _fillOpacity = Just 1.0
             {-, _transform = Just $ mempty { _transformE = 450-}
                                          {-, _transformF = 0-}
                                          {-}-}
             }

    go _ _ SvgNone = return ()
    go ctxt attr (Group groupAttr subTrees) =
        mapM_ (go context' attr') subTrees
      where attr' = attr <> groupAttr
            context' = mergeContext ctxt groupAttr

    go ctxt attr (Rectangle pAttr p w h) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' p
          w' = lineariseLength context' w
          h' = lineariseLength context' h
          rect = withTransform info $ rectangle p' w' h'
      filler info rect
      stroker info rect

    go ctxt attr (Circle pAttr p r) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' p
          r' = lineariseLength context' r
          c = withTransform info $ circle p' r'
      filler info c
      stroker info c

    go ctxt attr (Ellipse pAttr p rx ry) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' p
          rx' = lineariseXLength context' rx
          ry' = lineariseYLength context' ry
          c = withTransform info $ ellipse p' rx' ry'
      filler info c
      stroker info c

    go ctxt attr (Line pAttr p1 p2) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p1' = linearisePoint context' p1
          p2' = linearisePoint context' p2
      stroker info . withTransform info $ line p1' p2'

    go _ctxt attr (Path pAttr path) = do
      let info = attr <> pAttr
          primitives =
              withTransform info $ svgPathToPrimitives path
      filler info primitives
      stroker info primitives

