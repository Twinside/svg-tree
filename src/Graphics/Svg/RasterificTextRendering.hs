{-# LANGUAGE TupleSections #-}
module Graphics.Svg.RasterificTextRendering( renderText ) where

import Control.Monad( foldM )
import Control.Monad.IO.Class( liftIO )
import Control.Monad.Identity( Identity )
import Control.Monad.Trans.State.Strict( execState
                                       , StateT
                                       , modify
                                       , get
                                       , put
                                       , gets )
import Control.Applicative( (<$>), (<*>), (<|>) )
import Codec.Picture( PixelRGBA8( .. ) )
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid( mappend, mempty, (<>), Last( .. ), First( .. ) )
import Data.Maybe( fromMaybe )
import qualified Data.Text as T
import Graphics.Rasterific.Linear( (^+^), (^-^) )
import Graphics.Rasterific hiding ( Path, Line, Texture, transform )
import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Outline as RO
import Graphics.Rasterific.Immediate
import qualified Graphics.Rasterific.Transformations as RT
import Graphics.Rasterific.PathWalker
import Graphics.Text.TrueType
import Graphics.Svg.Types
import Graphics.Svg.RenderContext
import Graphics.Svg.PathConverter
{-import Graphics.Svg.XmlParser-}

import Debug.Trace
import Text.Printf

loadFont :: FilePath -> IODraw (Maybe Font)
loadFont path = do
  loaded <- get
  case M.lookup path loaded of
    Just v -> return . trace (printf "fetching cached:%s" path) $ Just v
    Nothing -> do
      file <- liftIO . trace (printf "Loading file:%s" path) $ loadFontFile path
      case file of
        Left _ -> return Nothing
        Right f -> do
          put $ M.insert path f loaded
          return $ Just f

data RenderableString px = RenderableString
    { _renderableAttributes :: !DrawAttributes
    , _renderableSize       :: !Float
    , _renderableFont       :: !Font
    , _renderableString     :: ![(Char, CharInfo px)]
    }

data CharInfo px = CharInfo
  { _charX  :: Maybe Number
  , _charY  :: Maybe Number
  , _charDx :: Maybe Number
  , _charDy :: Maybe Number
  , _charRotate :: Maybe Float
  , _charStroke :: Maybe (Float, R.Texture px, R.Join, (R.Cap, R.Cap))
  }

emptyCharInfo :: CharInfo px
emptyCharInfo = CharInfo
  { _charX      = Nothing
  , _charY      = Nothing
  , _charDx     = Nothing
  , _charDy     = Nothing
  , _charRotate = Nothing
  , _charStroke = Nothing
  }

propagateTextInfo :: TextInfo -> TextInfo -> TextInfo
propagateTextInfo parent current = TextInfo
  { _textInfoX = combine _textInfoX
  , _textInfoY = combine _textInfoY
  , _textInfoDX = combine _textInfoDX
  , _textInfoDY = combine _textInfoDY
  , _textInfoRotate = combine _textInfoRotate
  , _textInfoLength = _textInfoLength current
  }
  where
    combine f = case f current of
      [] -> f parent
      lst -> lst

textInfoRests :: TextInfo -> TextInfo -> TextInfo
              -> TextInfo
textInfoRests this parent sub = TextInfo
    { _textInfoX      = decideWith _textInfoX
    , _textInfoY      = decideWith _textInfoY
    , _textInfoDX     = decideWith _textInfoDX
    , _textInfoDY     = decideWith _textInfoDY
    , _textInfoRotate = decideWith _textInfoRotate
    , _textInfoLength = _textInfoLength parent
    }
  where
    decideWith f = decide (f this) (f parent) (f sub)

    decide []   _ ssub = ssub 
    decide  _ top    _ = top

unconsTextInfo :: RenderContext -> DrawAttributes -> TextInfo
               -> IODraw (CharInfo PixelRGBA8, TextInfo)
unconsTextInfo ctxt attr nfo = do
  texture <- textureOf ctxt attr _strokeColor _strokeOpacity
  return (charInfo texture, restText)
 where
  unconsInf lst = case lst of
     []     -> (Nothing, [])
     (x:xs) -> (Just x, xs)

  (xC, xRest) = unconsInf $ _textInfoX nfo
  (yC, yRest) = unconsInf $ _textInfoY nfo
  (dxC, dxRest) = unconsInf $ _textInfoDX nfo
  (dyC, dyRest) = unconsInf $ _textInfoDY nfo
  (rotateC, rotateRest) = unconsInf $ _textInfoRotate nfo

  restText = TextInfo
    { _textInfoX      = xRest
    , _textInfoY      = yRest
    , _textInfoDX     = dxRest
    , _textInfoDY     = dyRest
    , _textInfoRotate = rotateRest
    , _textInfoLength = _textInfoLength nfo
    }

  width =
     lineariseLength ctxt attr <$> getLast (_strokeWidth attr)

  charInfo tex = CharInfo
    { _charX = xC
    , _charY = yC
    , _charDx = dxC
    , _charDy = dyC
    , _charRotate = rotateC
    , _charStroke =
        (,, joinOfSvg attr, capOfSvg attr) <$> width <*> tex
    }

repeatLast :: [a] -> [a]
repeatLast = go where
  go lst = case lst of
    [] -> []
    [x] -> repeat x
    (x:xs) -> x : go xs

infinitizeTextInfo :: TextInfo -> TextInfo
infinitizeTextInfo nfo =
    nfo { _textInfoRotate = repeatLast $ _textInfoRotate nfo }


-- | Monadic version of mapAccumL
mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining funcction
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return    (s2, x' : xs')

mixWithRenderInfo :: RenderContext -> DrawAttributes
                  -> TextInfo -> String
                  -> IODraw (TextInfo, [(Char, CharInfo PixelRGBA8)])
mixWithRenderInfo ctxt attr = mapAccumLM go where
  go info c = do
    (thisInfo, rest) <- unconsTextInfo ctxt attr info
    return (rest, (c, thisInfo))


data LetterTransformerState = LetterTransformerState 
    { _charactersInfos      :: ![CharInfo PixelRGBA8]
    , _characterCurrent     :: !(CharInfo PixelRGBA8)
    , _currentCharDelta     :: !R.Point
    , _currentAbsoluteDelta :: !R.Point
    , _currentDrawing       :: Drawing PixelRGBA8 ()
    , _stringBounds         :: !PlaneBound
    }

type GlyphPlacer = StateT LetterTransformerState Identity

unconsCurrentLetter :: GlyphPlacer ()
unconsCurrentLetter = modify $ \s ->
  case _charactersInfos s of
    [] -> s
    (x:xs) -> s { _charactersInfos = xs
                , _characterCurrent = x
                }

prepareCharRotation :: CharInfo px -> R.PlaneBound -> RT.Transformation
prepareCharRotation info bounds = case _charRotate info of
  Nothing -> mempty
  Just angle -> RT.rotateCenter (toRadian angle) lowerLeftCorner
      where
        lowerLeftCorner = boundLowerLeftCorner bounds

prepareCharTranslation :: RenderContext -> CharInfo px -> R.PlaneBound
                       -> R.Point -> R.Point
                       -> (R.Point, R.Point, RT.Transformation)
prepareCharTranslation ctxt info bounds prevDelta prevAbsolute = go where
  lowerLeftCorner = boundLowerLeftCorner bounds
  toRPoint a b = linearisePoint ctxt mempty (a, b)
  mzero = Just $ Num 0
  V2 pmx pmy = Just . Num <$> prevAbsolute

  mayForcedPoint = case (_charX info, _charY info) of
    (Nothing, Nothing) -> Nothing
    (mx, my) -> toRPoint <$> (mx <|> pmx) <*> (my <|> pmy)

  delta = fromMaybe 0 $
    toRPoint <$> (_charDx info <|> mzero)
             <*> (_charDy info <|> mzero)

  go = case mayForcedPoint of
    Nothing ->
      let newDelta = prevDelta ^+^ delta
          trans = RT.translate $ newDelta ^+^ prevAbsolute in
      (newDelta, prevAbsolute, trans)

    Just p ->
      let newDelta = prevDelta ^+^ delta
          positionDelta = p ^-^ lowerLeftCorner
          trans = RT.translate $ positionDelta ^+^ newDelta in
      (newDelta, positionDelta, trans)

pixelToPt :: Float -> Float
pixelToPt a = a / 1.25

transformPlaceGlyph :: RenderContext
                    -> RT.Transformation
                    -> R.PlaneBound
                    -> DrawOrder PixelRGBA8
                    -> GlyphPlacer ()
transformPlaceGlyph ctxt pathTransformation bounds order = do
  unconsCurrentLetter 
  info <- gets _characterCurrent
  delta <- gets _currentCharDelta
  absoluteDelta <- gets _currentAbsoluteDelta
  let rotateTrans = prepareCharRotation info bounds
      (newDelta, newAbsolute, placement) =
        prepareCharTranslation ctxt info bounds delta absoluteDelta
      finalTrans = pathTransformation <> placement <> rotateTrans
      newGeometry =
          R.transform (RT.applyTransformation finalTrans) $ _orderPrimitives order
      newOrder = order { _orderPrimitives = newGeometry }

        
      stroking Nothing = return ()
      stroking (Just (w, texture, rjoin, cap)) =
          orderToDrawing $ newOrder {
            _orderPrimitives = stroker <$> _orderPrimitives newOrder,
            _orderTexture = texture
          }
         where
           stroker = RO.strokize w rjoin cap

  modify $ \s -> s
    { _currentCharDelta = newDelta
    , _currentAbsoluteDelta = newAbsolute
    , _stringBounds = _stringBounds s <> bounds
    , _currentDrawing = do
        _currentDrawing s
        orderToDrawing newOrder
        stroking $ _charStroke info
    }


prepareRenderableString :: RenderContext -> DrawAttributes -> Text
                        -> IODraw [RenderableString PixelRGBA8]
prepareRenderableString ctxt ini_attr root = -- trace (groom root) $
    fst <$> everySpan ini_attr mempty (_textRoot root) where

  everySpan attr originalInfo tspan =
      foldM (everyContent subAttr) (mempty, nfo) $ _spanContent tspan
    where
      subAttr = attr <> _spanDrawAttributes tspan
      nfo = propagateTextInfo originalInfo
          . infinitizeTextInfo
          $ _spanInfo tspan

  everyContent _attr (acc, info) (SpanTextRef _) = return (acc, info)
  everyContent attr (acc, info) (SpanSub thisSpan) = do
      let thisTextInfo = _spanInfo thisSpan
      (drawn, newInfo) <- everySpan attr info thisSpan
      return (acc <> drawn, textInfoRests thisTextInfo info newInfo)
  everyContent attr (acc, info) (SpanText txt) = do
    let fontFamilies = fromMaybe [] . getLast $ _fontFamily attr
        fontFilename = trace (show fontFamilies) $ getFirst $ F.foldMap fontFinder fontFamilies
    font <- loadFont $ fromMaybe "" fontFilename
    case font of
      Nothing -> return (acc, info)
      Just f -> do
        (info', str) <- mixWithRenderInfo ctxt attr info $ T.unpack txt
        let finalStr = RenderableString attr size f str
        return (acc <> [finalStr], info')
     
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

       fontFinder ff =
            First $ findFontInCache (_fontCache ctxt) descriptor
         where descriptor = FontDescriptor	 
                    { _descriptorFamilyName = T.pack ff
                    , _descriptorStyle = style }

anchorStringRendering :: TextAnchor -> LetterTransformerState
                      -> Drawing PixelRGBA8 ()
anchorStringRendering anchor st = case anchor of
    TextAnchorStart -> _currentDrawing st
    TextAnchorMiddle ->
        withTransformation (RT.translate (V2 (negate $ stringWidth / 2) 0)) $
            _currentDrawing st
    TextAnchorEnd ->
        withTransformation (RT.translate (V2 (- stringWidth) 0)) $ _currentDrawing st
  where
    stringWidth = boundWidth $ _stringBounds st

notWhiteSpace :: (Char, a) -> Bool
notWhiteSpace (c, _) = c /= ' ' && c /= '\t'

initialLetterTransformerState :: [RenderableString PixelRGBA8] -> LetterTransformerState
initialLetterTransformerState str = LetterTransformerState 
  { _charactersInfos   =
      fmap snd . filter notWhiteSpace . concat $ _renderableString <$> str
  , _characterCurrent  = emptyCharInfo
  , _currentCharDelta  = V2 0 0
  , _currentAbsoluteDelta = V2 0 0
  , _currentDrawing    = mempty
  , _stringBounds = mempty
  }

executePlacer :: Monad m => PathDrawer m px -> [DrawOrder px] -> m ()
executePlacer placer = F.mapM_ exec where
  exec order | bounds == mempty = return ()
             | otherwise = placer mempty bounds order
    where
      bounds = F.foldMap (F.foldMap planeBounds)
             $ _orderPrimitives order

textureOf :: RenderContext
          -> DrawAttributes
          -> (DrawAttributes -> Last Texture)
          -> (DrawAttributes -> Maybe Float)
          -> IODraw (Maybe (R.Texture PixelRGBA8))
textureOf ctxt attr colorAccessor opacityAccessor =
  case getLast $ colorAccessor attr of
    Nothing -> return Nothing
    Just svgTexture ->
        prepareTexture ctxt attr svgTexture opacity []
      where opacity = fromMaybe 1.0 $ opacityAccessor attr
 
renderString :: RenderContext -> Maybe (Float, R.Path) -> TextAnchor
             -> [RenderableString PixelRGBA8]
             -> IODraw (Drawing PixelRGBA8 ())
renderString ctxt mayPath anchor str = do
  textRanges <- mapM toFillTextRange str

  case mayPath of
    Just (offset, path) ->
        return . pathPlacer offset path $ fillOrders textRanges
    Nothing -> return . linePlacer $ fillOrders textRanges
  where
    fillOrders =
      drawOrdersOfDrawing width height background
        . printTextRanges 0

    (mini, maxi) = _renderViewBox ctxt
    V2 width height = floor <$> (maxi ^-^ mini)
    background = PixelRGBA8 0 0 0 0

    pathPlacer offset path =
        anchorStringRendering anchor
            . flip execState (initialLetterTransformerState str)
            . drawOrdersOnPath (transformPlaceGlyph ctxt) offset 0 path

    linePlacer =
        anchorStringRendering anchor
            . flip execState (initialLetterTransformerState str)
            . executePlacer (transformPlaceGlyph ctxt)
      
    toFillTextRange renderable = do
      mayTexture <- textureOf ctxt (_renderableAttributes renderable)
                        _fillColor _fillOpacity 
      return TextRange
        { _textFont = _renderableFont renderable
        , _textSize = floor . pixelToPt $ _renderableSize renderable
        , _text     = fst <$> _renderableString renderable
        , _textTexture = mayTexture
        }

startOffsetOfPath :: DrawAttributes -> R.Path -> Number
                  -> Float
startOffsetOfPath _ _ (Num i) = i
startOffsetOfPath attr _ (Em i) = emTransform attr i
startOffsetOfPath _ path (Percent p) =
    p * RO.approximatePathLength path

renderText :: RenderContext
           -> DrawAttributes
           -> Maybe TextPath
           -> Text
           -> IODraw (Drawing PixelRGBA8 ())
renderText ctxt attr ppath stext =
  prepareRenderableString ctxt attr stext >>= renderString ctxt pathInfo anchor
  where
    renderPath =
      svgPathToRasterificPath False . _textPathData <$> ppath

    offset = do
      rpath <- renderPath
      mayOffset <- _textPathStartOffset <$> ppath
      return . (\a -> trace ("OFFSET: " ++ show a) a) $ startOffsetOfPath attr rpath mayOffset

    pathInfo = (,) <$> (offset <|> return 0) <*> renderPath

    anchor = fromMaybe TextAnchorStart
           . getLast
           . _textAnchor
           . mappend attr
           . _spanDrawAttributes $ _textRoot stext

