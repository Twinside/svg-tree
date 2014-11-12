{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Svg.RasterificRender where

import Data.Monoid( mappend )
import Control.Monad( foldM )
import Control.Monad.IO.Class( liftIO )
import Control.Monad.Identity( Identity )
import Control.Monad.Trans.Class( lift )
import Control.Monad.Trans.State.Strict( runStateT
                                       , StateT
                                       , modify
                                       , get
                                       , put
                                       , gets )
import Control.Applicative( (<$>), (<*>), pure )
import Codec.Picture( Image, PixelRGBA8( .. ) )
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid( mempty, (<>), Last( .. ), First( .. ) )
import Data.Maybe( fromMaybe )
import Data.List( mapAccumL )
import qualified Data.Text as T
import Graphics.Rasterific.Linear( (^+^)
                                 , (^-^)
                                 , (^*)
                                 , norm
                                 , nearZero
                                 , zero )
import Graphics.Rasterific hiding ( Path, Line )
import qualified Graphics.Rasterific as R
import Graphics.Rasterific.Outline
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.PathWalker
import Graphics.Text.TrueType
import Graphics.Svg.Types
{-import Graphics.Svg.XmlParser-}

{-import Debug.Trace-}
{-import Text.Printf-}

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

svgPathToPrimitives :: Bool -> [SvgPath] -> [Primitive]
svgPathToPrimitives _ lst | isPathWithArc lst = []
svgPathToPrimitives shouldClose lst
    | shouldClose && not (nearZero $ norm (lastPoint ^-^ firstPoint)) =
        concat $ prims ++ [line lastPoint firstPoint]
    | otherwise = concat prims
  where
    ((lastPoint, _, firstPoint), prims) =
        mapAccumL go (zero, zero, zero) $ singularize lst

    go (latest, p, first) EndPath =
        ((first, p, first), line latest first)

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


renderSvgDocument :: FontCache -> Maybe (Int, Int) -> SvgDocument
                  -> IO (Image PixelRGBA8, LoadedFonts)
renderSvgDocument cache sizes doc = case sizes of
    Just s -> renderAtSize s
    Nothing -> renderAtSize $ svgDocumentSize doc
  where
    (x1, y1, x2, y2) = case (_svgViewBox doc, _svgWidth doc, _svgHeight doc) of
        (Just v,      _,      _) -> v
        (     _, Just w, Just h) -> (0, 0, w, h)
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
            withTransformation (scale (aw / vw) (ah / vh))
           where
             aw = fromIntegral actualWidth
             ah = fromIntegral actualHeight
    sizeFitter (V2 0 0, _) _ = id
    sizeFitter (p@(V2 xs ys), V2 xEnd yEnd) actualSize =
        withTransformation (translate (negate p)) .
            sizeFitter (zero, V2 (xEnd - xs) (yEnd - ys)) actualSize

    elements = _svgElements doc
    renderAtSize (w, h) = do
      let stateDraw = mapM (renderSvg emptyContext) elements
      (elems, s) <- runStateT stateDraw mempty
      let drawing = sizeFitter box (w, h) $ sequence_ elems
          img = renderDrawing w h white drawing
      return (img, s)

withInfo :: Monad m => (a -> Maybe b) -> a -> (b -> m ()) -> m ()
withInfo accessor val action = F.forM_ (accessor val) action

toRadian :: Float -> Float
toRadian v = v / 180 * pi

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

withTransform :: SvgDrawAttributes -> Drawing a () -> Drawing a ()
withTransform trans draw =
    case _transform trans of
       Nothing -> draw
       Just t -> withTransformation fullTrans draw
         where fullTrans = F.foldMap toTransformationMatrix t

data RenderContext = RenderContext
    { _initialViewBox :: (Point, Point)
    , _renderViewBox  :: (Point, Point)
    , _definitions    :: M.Map String SvgElement
    , _fontCache      :: FontCache
    }


type LoadedFonts = M.Map FilePath Font

type IODraw = StateT LoadedFonts IO

type ViewBox = (Point, Point)

fillAlphaCombine :: Float -> PixelRGBA8 -> PixelRGBA8
fillAlphaCombine opacity (PixelRGBA8 r g b a) =
    PixelRGBA8 r g b alpha
  where
    a' = fromIntegral a / 255.0
    alpha = floor . max 0 . min 255 $ opacity * a' * 255

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

withSvgTexture :: RenderContext -> SvgDrawAttributes
               -> SvgTexture -> Float
               -> [Primitive]
               -> Drawing PixelRGBA8 ()
withSvgTexture ctxt attr texture opacity prims =
  case prepareTexture ctxt attr texture opacity prims of
    Nothing -> return ()
    Just tex ->
      let method = fillMethodOfSvg attr in
      withTexture tex $ fillWithMethod method prims

filler :: RenderContext
       -> SvgDrawAttributes
       -> [Primitive]
       -> Drawing PixelRGBA8 ()
filler ctxt info primitives =
  withInfo (getLast . _fillColor) info $ \svgTexture ->
    withSvgTexture ctxt info svgTexture (_fillOpacity info) primitives

stroker :: RenderContext -> SvgDrawAttributes -> [Primitive]
        -> Drawing PixelRGBA8 ()
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

emTransform :: SvgDrawAttributes -> Float -> Float
emTransform attr n = case getLast $ _fontSize attr of
    Nothing -> 16 * n
    Just (SvgNum v) -> v * n
    Just _ -> 16 * n

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

viewBoxOfTree :: SvgTree -> Maybe (Int, Int, Int, Int)
viewBoxOfTree (Symbol g) = _svgGroupViewBox g
viewBoxOfTree _ = Nothing

loadFont :: FilePath -> IODraw (Maybe Font)
loadFont path = do
  loaded <- get
  case M.lookup path loaded of
    Just v -> return $ Just v
    Nothing -> do
      file <- liftIO $ loadFontFile path
      case file of
        Left _ -> return Nothing
        Right f -> do
          put $ M.insert path f loaded
          return $ Just f

data RenderableString = RenderableString
    { _renderableAttributes :: !SvgDrawAttributes
    , _renderableSize       :: !Float
    , _renderableFont       :: !Font
    , _renderableString     :: ![(Char, CharInfo)]
    }

mixWithRenderInfo :: SvgTextInfo -> String
                  -> (SvgTextInfo, [(Char, CharInfo)])
mixWithRenderInfo = mapAccumL go where
  go info c = (rest, (c, thisInfo))
    where
      (thisInfo, rest) = unconsTextInfo info


data LetterTransformerState = LetterTransformerState 
    { _charactersInfos  :: ![CharInfo]
    , _characterCurrent :: !CharInfo
    , _currentCharDelta  :: !Point
    }

type GlyphPlacer = StateT LetterTransformerState Identity

type PathLetterTransformer = PathWalkerT GlyphPlacer

unconsCurrentLetter :: GlyphPlacer ()
unconsCurrentLetter = modify $ \s ->
  case _charactersInfos s of
    [] -> s
    (x:xs) -> s { _charactersInfos = xs
                , _characterCurrent = x
                }

prepareCharRotation :: CharInfo -> PlaneBound -> Transformation
                    -> Transformation
prepareCharRotation info bounds = case _svgCharRotate info of
  Nothing -> id
  Just angle ->
    mappend $ rotateCenter (toRadian angle) lowerLeftCorner
      where
        lowerLeftCorner = boundLowerLeftCorner bounds

prepareCharTranslation :: RenderContext -> CharInfo -> PlaneBound
                       -> Point -> Transformation
                       -> (Point, Transformation)
prepareCharTranslation ctxt info bounds prevDelta nextTrans = go where
  lowerLeftCorner = boundLowerLeftCorner bounds
  toPoint a b = linearisePoint ctxt mempty (a, b)
  mayForcedPoint = toPoint <$> _svgCharX info <*> _svgCharY info
  mayDelta = toPoint <$> _svgCharDx info <*> _svgCharDy info
  mergeNext = mappend nextTrans
  go = case (mayForcedPoint, mayDelta) of
    (Nothing, Nothing) -> (prevDelta, nextTrans <> translate prevDelta)
    (Just p, Nothing) ->
        (prevDelta, mergeNext $ translate (p ^-^ lowerLeftCorner))
    (Nothing, Just delta) ->
        let newDelta = prevDelta ^+^ delta in
        (newDelta, mergeNext $ translate newDelta)
    (Just p, Just delta) ->
        let newDelta = prevDelta ^+^ delta
            trans = translate (p ^-^ lowerLeftCorner ^+^ newDelta) in
        (newDelta, mergeNext trans)

transformPlaceGlyph :: RenderContext -> Transformation -> [Primitive]
                    -> GlyphPlacer Transformation
transformPlaceGlyph ctxt trans prims = do
  unconsCurrentLetter 
  info <- gets _characterCurrent
  delta <- gets _currentCharDelta
  let bounds = F.foldMap planeBounds prims
      rotateTrans = prepareCharRotation info bounds trans
      (newDelta, finalTrans) =
        prepareCharTranslation ctxt info bounds delta rotateTrans 
  modify $ \s -> s { _currentCharDelta = newDelta }
  return finalTrans

renderText :: RenderContext -> R.Path -> [RenderableString]
           -> PathLetterTransformer ()
renderText ctxt path str = 
    runPathWalking
printTextRanges 0 ranges where
  ranges = toTextRange <$> str
  textureOf renderable = do
    let attr = _renderableAttributes renderable
    svgTexture <- getLast $ _fillColor attr
    prepareTexture ctxt attr svgTexture (_fillOpacity attr) []

  toTextRange renderable = TextRange
    { _textFont = _renderableFont renderable
    , _textSize = floor $ _renderableSize renderable
    , _text     = fst <$> _renderableString renderable
    , _textTexture = textureOf renderable
    }

prepareRenderableString :: RenderContext -> SvgDrawAttributes -> SvgText
                        -> IODraw [RenderableString]
prepareRenderableString ctxt ini_attr textRoot =
    fst <$> everySpan ini_attr mempty (_svgTextRoot textRoot) where

  everySpan attr originalInfo tspan =
      foldM (everyContent subAttr) (mempty, nfo) $ _svgSpanContent tspan
    where
      subAttr = attr <> _svgSpanDrawAttributes tspan
      nfo = infinitizeTextInfo $ _svgSpanInfo tspan

  everyContent _attr (acc, info) (SvgSpanTextRef _) = return (acc, info)
  everyContent attr (acc, info) (SvgSpanSub thisSpan) = do
      let thisTextInfo = _svgSpanInfo thisSpan
      (drawn, newInfo) <- everySpan attr info thisSpan
      return (acc <> drawn, textInfoRests thisTextInfo info newInfo)
  everyContent attr (acc, info) (SvgSpanText txt) = do
    let fontFamilies = fromMaybe [] . getLast $ _fontFamily attr
        fontFilename = getFirst $ F.foldMap fontFinder fontFamilies
    font <- loadFont $ fromMaybe "" fontFilename
    case font of
      Nothing -> return (acc, info)
      Just f ->
        let (acc', str) = mixWithRenderInfo info $ T.unpack txt
            finalStr = RenderableString attr size f str
        in
        return ([finalStr], acc')
     
     where
       size = case getLast $ _fontSize attr of
          Just v -> lineariseLength ctxt attr v
          Nothing -> 16

       noStyle = FontStyle
               { _fontStyleBold = False
               , _fontStyleItalic = False }
       italic = noStyle { _fontStyleItalic = True }

       style = case getLast $ _fontStyle attr of
         Nothing -> noStyle
         Just FontStyleNormal -> noStyle
         Just FontStyleItalic -> italic
         Just FontStyleOblique -> italic

       fontFinder fontFamily =
            First $ findFontInCache (_fontCache ctxt) descriptor
         where descriptor = FontDescriptor	 
                    { _descriptorFamilyName = T.pack fontFamily
                    , _descriptorStyle = style }

renderSvg :: RenderContext -> SvgTree -> IODraw (Drawing PixelRGBA8 ())
renderSvg initialContext = go initialContext initialAttr
  where
    initialAttr =
      mempty { _strokeWidth = Last $ Just (SvgNum 1.0)
             , _strokeLineCap = Last $ Just SvgCapButt
             , _strokeLineJoin = Last $ Just SvgJoinMiter
             , _strokeMiterLimit = Last $ Just 4.0
             , _strokeOpacity = 1.0
             , _fillColor = Last . Just . ColorRef $ PixelRGBA8 0 0 0 255
             , _fillOpacity = 1.0
             , _fillRule = Last $ Just SvgFillNonZero
             }

    fitUse ctxt attr use subTree =
      let origin = linearisePoint ctxt attr $ _svgUseBase use
          w = lineariseXLength ctxt attr <$> _svgUseWidth use
          h = lineariseYLength ctxt attr <$> _svgUseHeight use
      in
      case viewBoxOfTree subTree of
        Nothing -> withTransformation (translate origin)
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
          withTransformation $ translate origin
                            <> scale xScaleFactor yScaleFactor
                            <> translate (negate boxOrigin)


    go _ _ SvgNone = return mempty
    -- not handled yet
    go ctxt attr (TextArea _ stext) = do
      _ <- prepareRenderableString ctxt attr stext
      error "Muh"
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
            (0, 0) -> rectangle p' w' h'
            (v, 0) -> roundedRectangle p' w' h' v v
            (0, v) -> roundedRectangle p' w' h' v v
            (vx, vy) -> roundedRectangle p' w' h' vx vy

      return . withTransform pAttr $ do
        filler context' info rect
        stroker context' info rect

    go ctxt attr (Circle (SvgCircle pAttr p r)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          r' = lineariseLength context' info r
          c = circle p' r'
      return . withTransform pAttr $ do
        filler context' info c
        stroker context' info c

    go ctxt attr (Ellipse (SvgEllipse pAttr p rx ry)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          rx' = lineariseXLength context' info rx
          ry' = lineariseYLength context' info ry
          c = ellipse p' rx' ry'
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
      return . withTransform pAttr . stroker context' info $ line p1' p2'

    go ctxt attr (Path (SvgPathPrim pAttr path)) = do
      let info = attr <> pAttr
          strokePrimitives = svgPathToPrimitives False path
          fillPrimitives = svgPathToPrimitives True path
      return . withTransform pAttr $ do
        filler ctxt info fillPrimitives
        stroker ctxt info strokePrimitives

