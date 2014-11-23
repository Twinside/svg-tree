{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Svg.RasterificTextRendering( renderText ) where

import Data.Monoid( mappend )
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
import Data.Monoid( mempty, (<>), Last( .. ), First( .. ) )
import Data.Maybe( fromMaybe )
import Data.List( mapAccumL )
import qualified Data.Text as T
import Graphics.Rasterific.Linear( (^+^), (^-^) )
import Graphics.Rasterific hiding ( Path, Line, transform )
import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Outline as RO
import Graphics.Rasterific.Immediate
import Graphics.Rasterific.Transformations
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
    { _renderableAttributes :: !SvgDrawAttributes
    , _renderableSize       :: !Float
    , _renderableFont       :: !Font
    , _renderableString     :: ![(Char, CharInfo px)]
    }

data CharInfo px = CharInfo
  { _svgCharX  :: Maybe SvgNumber
  , _svgCharY  :: Maybe SvgNumber
  , _svgCharDx :: Maybe SvgNumber
  , _svgCharDy :: Maybe SvgNumber
  , _svgCharRotate :: Maybe Float
  , _svgCharStroke :: Maybe (Float, Texture px, R.Join, (R.Cap, R.Cap))
  }

emptyCharInfo :: CharInfo px
emptyCharInfo = CharInfo
  { _svgCharX      = Nothing
  , _svgCharY      = Nothing
  , _svgCharDx     = Nothing
  , _svgCharDy     = Nothing
  , _svgCharRotate = Nothing
  , _svgCharStroke = Nothing
  }

propagateTextInfo :: SvgTextInfo -> SvgTextInfo -> SvgTextInfo
propagateTextInfo parent current = SvgTextInfo
  { _svgTextInfoX = combine _svgTextInfoX
  , _svgTextInfoY = combine _svgTextInfoY
  , _svgTextInfoDX = combine _svgTextInfoDX
  , _svgTextInfoDY = combine _svgTextInfoDY
  , _svgTextInfoRotate = combine _svgTextInfoRotate
  , _svgTextInfoLength = _svgTextInfoLength current
  }
  where
    combine f = case f current of
      [] -> f parent
      lst -> lst

textInfoRests :: SvgTextInfo -> SvgTextInfo -> SvgTextInfo
              -> SvgTextInfo
textInfoRests this parent sub = SvgTextInfo
    { _svgTextInfoX      = decideWith _svgTextInfoX
    , _svgTextInfoY      = decideWith _svgTextInfoY
    , _svgTextInfoDX     = decideWith _svgTextInfoDX
    , _svgTextInfoDY     = decideWith _svgTextInfoDY
    , _svgTextInfoRotate = decideWith _svgTextInfoRotate
    , _svgTextInfoLength = _svgTextInfoLength parent
    }
  where
    decideWith f = decide (f this) (f parent) (f sub)

    decide []   _ ssub = ssub 
    decide  _ top    _ = top

unconsTextInfo :: RenderContext -> SvgDrawAttributes -> SvgTextInfo
               -> (CharInfo PixelRGBA8, SvgTextInfo)
unconsTextInfo ctxt attr nfo = (charInfo, restText) where
  unconsInf lst = case lst of
     []     -> (Nothing, [])
     (x:xs) -> (Just x, xs)

  (xC, xRest) = unconsInf $ _svgTextInfoX nfo
  (yC, yRest) = unconsInf $ _svgTextInfoY nfo
  (dxC, dxRest) = unconsInf $ _svgTextInfoDX nfo
  (dyC, dyRest) = unconsInf $ _svgTextInfoDY nfo
  (rotateC, rotateRest) = unconsInf $ _svgTextInfoRotate nfo

  restText = SvgTextInfo
    { _svgTextInfoX      = xRest
    , _svgTextInfoY      = yRest
    , _svgTextInfoDX     = dxRest
    , _svgTextInfoDY     = dyRest
    , _svgTextInfoRotate = rotateRest
    , _svgTextInfoLength = _svgTextInfoLength nfo
    }

  texture = textureOf ctxt attr _strokeColor _strokeOpacity
  width =
     lineariseLength ctxt attr <$> getLast (_strokeWidth attr)

  charInfo = CharInfo
    { _svgCharX = xC
    , _svgCharY = yC
    , _svgCharDx = dxC
    , _svgCharDy = dyC
    , _svgCharRotate = rotateC
    , _svgCharStroke =
        (,, joinOfSvg attr, capOfSvg attr) <$> width <*> texture
    }

repeatLast :: [a] -> [a]
repeatLast = go where
  go lst = case lst of
    [] -> []
    [x] -> repeat x
    (x:xs) -> x : go xs

infinitizeTextInfo :: SvgTextInfo -> SvgTextInfo
infinitizeTextInfo nfo =
    nfo { _svgTextInfoRotate = repeatLast $ _svgTextInfoRotate nfo }


mixWithRenderInfo :: RenderContext -> SvgDrawAttributes
                  -> SvgTextInfo -> String
                  -> (SvgTextInfo, [(Char, CharInfo PixelRGBA8)])
mixWithRenderInfo ctxt attr = mapAccumL go where
  go info c = (rest, (c, thisInfo))
    where
      (thisInfo, rest) = unconsTextInfo ctxt attr info


data LetterTransformerState = LetterTransformerState 
    { _charactersInfos      :: ![CharInfo PixelRGBA8]
    , _characterCurrent     :: !(CharInfo PixelRGBA8)
    , _currentCharDelta     :: !Point
    , _currentAbsoluteDelta :: !Point
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

prepareCharRotation :: CharInfo px -> PlaneBound -> Transformation
prepareCharRotation info bounds = case _svgCharRotate info of
  Nothing -> mempty
  Just angle -> rotateCenter (toRadian angle) lowerLeftCorner
      where
        lowerLeftCorner = boundLowerLeftCorner bounds

prepareCharTranslation :: RenderContext -> CharInfo px -> PlaneBound
                       -> Point -> Point
                       -> (Point, Point, Transformation)
prepareCharTranslation ctxt info bounds prevDelta prevAbsolute = go where
  lowerLeftCorner = boundLowerLeftCorner bounds
  toPoint a b = linearisePoint ctxt mempty (a, b)
  mzero = Just $ SvgNum 0
  V2 pmx pmy = Just . SvgNum <$> prevAbsolute

  mayForcedPoint = case (_svgCharX info, _svgCharY info) of
    (Nothing, Nothing) -> Nothing
    (mx, my) -> toPoint <$> (mx <|> pmx) <*> (my <|> pmy)

  delta = fromMaybe 0 $
    toPoint <$> (_svgCharDx info <|> mzero)
            <*> (_svgCharDy info <|> mzero)

  go = case mayForcedPoint of
    Nothing ->
      let newDelta = prevDelta ^+^ delta
          trans = translate $ newDelta ^+^ prevAbsolute in
      (newDelta, prevAbsolute, trans)

    Just p ->
      let newDelta = prevDelta ^+^ delta
          positionDelta = p ^-^ lowerLeftCorner
          trans = translate $ positionDelta ^+^ newDelta in
      (newDelta, positionDelta, trans)

pixelToPt :: Float -> Float
pixelToPt a = a / 1.25

transformPlaceGlyph :: RenderContext
                    -> Transformation
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
          R.transform (applyTransformation finalTrans) $ _orderPrimitives order
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
        stroking $ _svgCharStroke info
    }


prepareRenderableString :: RenderContext -> SvgDrawAttributes -> SvgText
                        -> IODraw [RenderableString PixelRGBA8]
prepareRenderableString ctxt ini_attr textRoot = -- trace (groom textRoot) $
    fst <$> everySpan ini_attr mempty (_svgTextRoot textRoot) where

  everySpan attr originalInfo tspan =
      foldM (everyContent subAttr) (mempty, nfo) $ _svgSpanContent tspan
    where
      subAttr = attr <> _svgSpanDrawAttributes tspan
      nfo = propagateTextInfo originalInfo
          . infinitizeTextInfo
          $ _svgSpanInfo tspan

  everyContent _attr (acc, info) (SvgSpanTextRef _) = return (acc, info)
  everyContent attr (acc, info) (SvgSpanSub thisSpan) = do
      let thisTextInfo = _svgSpanInfo thisSpan
      (drawn, newInfo) <- everySpan attr info thisSpan
      return (acc <> drawn, textInfoRests thisTextInfo info newInfo)
  everyContent attr (acc, info) (SvgSpanText txt) = do
    let fontFamilies = fromMaybe [] . getLast $ _fontFamily attr
        fontFilename = trace (show fontFamilies) $ getFirst $ F.foldMap fontFinder fontFamilies
    font <- loadFont $ fromMaybe "" fontFilename
    case font of
      Nothing -> return (acc, info)
      Just f ->
        let (info', str) = mixWithRenderInfo ctxt attr info $ T.unpack txt
            finalStr = RenderableString attr size f str
        in
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

anchorStringRendering :: SvgTextAnchor -> LetterTransformerState
                      -> Drawing PixelRGBA8 ()
anchorStringRendering anchor st = case anchor of
    SvgTextAnchorStart -> _currentDrawing st
    SvgTextAnchorMiddle ->
        withTransformation (translate (V2 (negate $ stringWidth / 2) 0)) $
            _currentDrawing st
    SvgTextAnchorEnd ->
        withTransformation (translate (V2 (- stringWidth) 0)) $ _currentDrawing st
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
          -> SvgDrawAttributes
          -> (SvgDrawAttributes -> Last SvgTexture)
          -> (SvgDrawAttributes -> Float)
          -> Maybe (Texture PixelRGBA8)
textureOf ctxt attr colorAccessor opacityAccessor = do
  svgTexture <- getLast $ colorAccessor attr
  prepareTexture ctxt attr svgTexture (opacityAccessor attr) []
 
renderString :: RenderContext -> Maybe R.Path -> SvgTextAnchor
             -> [RenderableString PixelRGBA8]
             -> Drawing PixelRGBA8 ()
renderString ctxt mayPath anchor str
  | Just path <- mayPath = pathPlacer path fillOrders
  | otherwise = linePlacer fillOrders
  where
    fillOrders = drawOrdersOfDrawing width height background
               . printTextRanges 0
               $ toFillTextRange <$> str

    (mini, maxi) = _renderViewBox ctxt
    V2 width height = floor <$> (maxi ^-^ mini)
    background = PixelRGBA8 0 0 0 0

    pathPlacer path =
        anchorStringRendering anchor
            . flip execState (initialLetterTransformerState str)
            . drawOrdersOnPath (transformPlaceGlyph ctxt)
                            R.AlignOnMiddle 0 path
    linePlacer =
        anchorStringRendering anchor
            . flip execState (initialLetterTransformerState str)
            . executePlacer (transformPlaceGlyph ctxt)
      
    toFillTextRange renderable = TextRange
      { _textFont = _renderableFont renderable
      , _textSize = floor . pixelToPt $ _renderableSize renderable
      , _text     = fst <$> _renderableString renderable
      , _textTexture =
          textureOf ctxt
            (_renderableAttributes renderable)
            _fillColor
            _fillOpacity 
      }


renderText :: RenderContext
           -> SvgDrawAttributes
           -> Maybe SvgTextPath
           -> SvgText
           -> IODraw (Drawing PixelRGBA8 ())
renderText ctxt attr ppath stext =
  renderString ctxt renderPath anchor <$> prepareRenderableString ctxt attr stext
  where
    renderPath =
      svgPathToRasterificPath False . _svgTextPathData <$> ppath

    anchor = fromMaybe SvgTextAnchorStart
           . getLast
           . _textAnchor
           . mappend attr
           . _svgSpanDrawAttributes $ _svgTextRoot stext

