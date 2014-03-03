import Control.Applicative( (<$>), pure )
import Data.Monoid( Monoid( .. ), (<>) )
import Graphics.Svg
import Graphics.Svg.Types
import Graphics.Rasterific hiding ( Path )
import Graphics.Rasterific.Texture
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Linear( (^-^), (^+^), (^*) )
import Data.List( mapAccumL )

import Debug.Trace

toPoint :: (Float, Float) -> V2 Float
toPoint (a, b) = V2 a b

{-ofPoint :: V2 Float -> (Float, Float)-}
{-ofPoint (V2 a b) = (a, b)-}

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

pointTransform :: Transform -> V2 Float -> V2 Float
pointTransform (Transform a c e
                          b d f) (V2 x y) =
    V2 (a * x + c * y + e)
       (b * x + d * y + f)

inverseTransform :: Transform -> Transform
inverseTransform (Transform a c e
                            b d f) =
    Transform a' c' e' b' d' f'
  where det = a * d - b * c
        a' = d / det
        c' = (- c) / det
        e' = (c * f - e * d) / det

        b' = (- b) / det
        d' = a / det
        f' = (e * b - a * f) / det

withInfo :: Monad m => (a -> Maybe b) -> a -> (b -> m ()) -> m ()
withInfo accessor val action =
  case accessor val of
    Nothing -> return ()
    Just v -> action v

withTransform :: SvgDrawAttributes -> [Primitive] -> [Primitive]
withTransform trans prims =
    case _transform trans of
       Nothing -> prims
       Just t ->
           (\a -> trace ("====== shifted\n" ++ show a) a)
                . trace ("====== transform\n" ++ show t)
                . trace ("====== inverse transform\n" ++ show it)
                . trace ("====== prims\n" ++ show prims)
                {-. transform (pointTransform t) <$> prims-}
                . transform (^+^ V2 450 200) <$> prims
                {-. transform (^* 3.0)-}
          where it = inverseTransform t


renderSvg :: SvgTree -> Drawing PixelRGBA8 ()
renderSvg = go initialAttr
  where
    initialAttr =
      mempty { _strokeWidth = Just 1.0
             {-, _transform = Just $ mempty { _transformE = 450-}
                                          {-, _transformF = 0-}
                                          {-}-}
             }

    go _ SvgNone = return ()
    go attr (Group groupAttr subTrees) =
        mapM_ (go (attr <> groupAttr)) subTrees
    go attr (Path pAttr path) = do
      let info = attr <> pAttr
          primitives =
              withTransform info $ svgPathToPrimitives path

      withInfo _fillColor info $ \c ->
        withTexture (uniformTexture c) $ fill primitives

      withInfo _strokeWidth info $ \swidth ->
        withInfo _strokeColor info $ \color ->
          withTexture (uniformTexture color) $
            stroke swidth
                (JoinMiter 0)
                (CapStraight 0, CapStraight 0)
                primitives

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

    go (_, _, _) (MoveTo OriginAbsolute (p:_)) =
        ((pp, pp, pp), []) where pp = toPoint p
    go (o, _, _) (MoveTo OriginRelative (p:_)) =
        ((pp, pp, pp), []) where pp = o ^+^ toPoint p

    go (o@(V2 _ y), _, fp) (HorizontalTo OriginAbsolute (c:_)) =
        ((p, p, fp), line o p) where p = V2 c y
    go (o@(V2 x y), _, fp) (HorizontalTo OriginRelative (c:_)) =
        ((p, p, fp), line o p) where p = V2 (x + c) y

    go (o@(V2 x _), _, fp) (VerticalTo OriginAbsolute (c:_)) =
        ((p, p, fp), line o p) where p = V2 x c
    go (o@(V2 x y), _, fp) (VerticalTo OriginRelative (c:_)) =
        ((p, p, fp), line o p) where p = V2 x (c + y)

    go (o, _, fp) (LineTo OriginRelative (c:_)) =
        ((p, p, fp), line o p) where p = o ^+^ toPoint c

    go (o, _, fp) (LineTo OriginAbsolute (pp:_)) =
        ((p, p, fp), line o p) where p = toPoint pp

    go (o, _, fp) (CurveTo OriginAbsolute ((c1, c2, e):_)) =
        ((e', c2', fp), [CubicBezierPrim $ CubicBezier o c1' c2' e'])
      where c1' = toPoint c1
            c2' = toPoint c2
            e' = toPoint e

    go (o, _, fp) (CurveTo OriginRelative ((c1, c2, e):_)) =
        ((e', c2', fp), [CubicBezierPrim $ CubicBezier o c1' c2' e'])
      where c1' = o ^+^ toPoint c1
            c2' = o ^+^ toPoint c2
            e' = o ^+^ toPoint e

    go (o, control, fp) (SmoothCurveTo OriginAbsolute ((c2, e):_)) =
        ((e', c2', fp), [CubicBezierPrim $ CubicBezier o c1' c2' e'])
      where c1' = o ^* 2 ^-^ control
            c2' = toPoint c2
            e' = toPoint e

    go (o, control, fp) (SmoothCurveTo OriginRelative ((c2, e):_)) =
        ((e', c2', fp), [CubicBezierPrim $ CubicBezier o c1' c2' e'])
      where c1' = o ^* 2 ^-^ control
            c2' = o ^+^ toPoint c2
            e' = o ^+^ toPoint e

    go (o, _, fp) (QuadraticBezier OriginAbsolute ((c1, e):_)) =
        ((e', c1', fp), [BezierPrim $ Bezier o c1' e'])
      where e' = toPoint e
            c1' = toPoint c1

    go (o, _, fp) (QuadraticBezier OriginRelative ((c1, e):_)) =
        ((e', c1', fp), [BezierPrim $ Bezier o c1' e'])
      where c1' = o ^+^ toPoint c1
            e' = o ^+^ toPoint e

    go (o, control, fp)
       (SmoothQuadraticBezierCurveTo OriginAbsolute (e:_)) =
       ((e', c1', fp), [BezierPrim $ Bezier o c1' e'])
      where c1' = o ^* 2 ^-^ control
            e' = toPoint e

    go (o, control, fp)
       (SmoothQuadraticBezierCurveTo OriginRelative (e:_)) =
       ((e', c1', fp), [BezierPrim $ Bezier o c1' e'])
      where c1' = o ^* 2 ^-^ control
            e' = o ^+^ toPoint e

    go _ (ElipticalArc _ _) = error "Unimplemented"

renderSvgDocument :: FilePath -> SvgDocument -> IO ()
renderSvgDocument path doc = writePng path drawing
  where
    white = PixelRGBA8 255 255 255 255
    drawing = renderDrawing 900 900 white .
        mapM_ renderSvg $ _svgElements doc

main :: IO ()
main = do
    let _ = svgPathToPrimitives undefined
    f <- loadSvgFile "tiger.svg"
    {-f <- loadSvgFile "tigerlight.svg"-}
    case f of
       Nothing -> putStrLn "Error while loading SVG"
       Just doc -> renderSvgDocument "tiger.png" doc

